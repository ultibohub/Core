{
Ultibo Mouse interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

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

 USB HID Device Class Definition 1_11.pdf
 
   http://www.usb.org/developers/hidpage/HID1_11.pdf

 USB HID Usage Tables 1_12v2.pdf

   http://www.usb.org/developers/hidpage/Hut1_12v2.pdf
 
Mouse Devices
=============

 This unit provides both the Mouse device interface and the generic USB HID mouse driver.

USB Mouse Devices
=================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Mouse;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,USB,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Mouse specific constants}
 MOUSE_NAME_PREFIX = 'Mouse';    {Name prefix for Mouse Devices}
 
 {Mouse Device Types}
 MOUSE_TYPE_NONE     = 0;
 MOUSE_TYPE_USB      = 1;
 MOUSE_TYPE_PS2      = 2;
 MOUSE_TYPE_SERIAL   = 3;
 
 MOUSE_TYPE_MAX      = 3;
 
 {Mouse Type Names}
 MOUSE_TYPE_NAMES:array[MOUSE_TYPE_NONE..MOUSE_TYPE_MAX] of String = (
  'MOUSE_TYPE_NONE',
  'MOUSE_TYPE_USB',
  'MOUSE_TYPE_PS2',
  'MOUSE_TYPE_SERIAL');
 
 {Mouse Device States}
 MOUSE_STATE_DETACHED  = 0;
 MOUSE_STATE_DETACHING = 1;
 MOUSE_STATE_ATTACHING = 2;
 MOUSE_STATE_ATTACHED  = 3;
 
 MOUSE_STATE_MAX       = 3;
 
 {Mouse State Names}
 MOUSE_STATE_NAMES:array[MOUSE_STATE_DETACHED..MOUSE_STATE_MAX] of String = (
  'MOUSE_STATE_DETACHED',
  'MOUSE_STATE_DETACHING',
  'MOUSE_STATE_ATTACHING',
  'MOUSE_STATE_ATTACHED');
 
 {Mouse Device Flags}
 MOUSE_FLAG_NONE         = $00000000;
 MOUSE_FLAG_NON_BLOCK    = $00000001; {If set device reads are non blocking (Also supported in Flags parameter of MouseReadEx)}
 MOUSE_FLAG_DIRECT_READ  = $00000002; {If set device writes mouse data to its local buffer and which must be read using MouseDeviceRead}
 MOUSE_FLAG_SWAP_BUTTONS = $00000004; {If set left and right mouse buttons will be swapped in mouse data}
 MOUSE_FLAG_PEEK_BUFFER  = $00000008; {Peek at the buffer to see if any data is available, don't remove it (Used only in Flags parameter of MouseReadEx)}
 
 {Flags supported by MOUSE_CONTROL_GET/SET/CLEAR_FLAG}
 MOUSE_FLAG_MASK = MOUSE_FLAG_NON_BLOCK or MOUSE_FLAG_DIRECT_READ or MOUSE_FLAG_SWAP_BUTTONS;
 
 {Mouse Device Control Codes}
 MOUSE_CONTROL_GET_FLAG         = 1;  {Get Flag}
 MOUSE_CONTROL_SET_FLAG         = 2;  {Set Flag}
 MOUSE_CONTROL_CLEAR_FLAG       = 3;  {Clear Flag}
 MOUSE_CONTROL_FLUSH_BUFFER     = 4;  {Flush Buffer}
 //To Do //Acceleration etc

 {Mouse Buffer Size}
 MOUSE_BUFFER_SIZE = 512; 
 
 {Mouse Data Definitions (Values for TMouseData.Buttons)}
 MOUSE_LEFT_BUTTON    =  $0001; {The Left mouse button is pressed}
 MOUSE_RIGHT_BUTTON   =  $0002; {The Right mouse button is pressed}
 MOUSE_MIDDLE_BUTTON  =  $0004; {The Middle mouse button is pressed}
 MOUSE_SIDE_BUTTON    =  $0008; {The Side mouse button is pressed}
 MOUSE_EXTRA_BUTTON   =  $0010; {The Extra mouse button is pressed}
 MOUSE_TOUCH_BUTTON   =  $0020; {The Touch screen is being touched}
 MOUSE_ABSOLUTE_X     =  $0040; {The OffsetX value is absolute not relative}
 MOUSE_ABSOLUTE_Y     =  $0080; {The OffsetY value is absolute not relative}
 MOUSE_ABSOLUTE_WHEEL =  $0100; {The OffsetWheel value is absolute not relative}
 
 {Mouse logging}
 MOUSE_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Mouse debugging messages}
 MOUSE_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Mouse informational messages, such as a device being attached or detached}
 MOUSE_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Mouse error messages}
 MOUSE_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Mouse messages}

var 
 MOUSE_DEFAULT_LOG_LEVEL:LongWord = MOUSE_LOG_LEVEL_DEBUG; {Minimum level for Mouse messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {Mouse logging}
 MOUSE_LOG_ENABLED:Boolean; 

{==============================================================================}
const
 {USB Mouse specific constants}
 USBMOUSE_DRIVER_NAME = 'USB Mouse Driver (HID boot protocol)'; {Name of USB mouse driver}

 USBMOUSE_MOUSE_DESCRIPTION = 'USB HID Mouse'; {Description of USB mouse device}
 
 {HID Interface Subclass types (See USB HID v1.11 specification)}
 USB_HID_SUBCLASS_BOOT           = 1;     {Section 4.2}
 
 {HID Interface Protocol types (See USB HID v1.11 specification)}
 USB_HID_BOOT_PROTOCOL_KEYBOARD  = 1;     {Section 4.3}
 USB_HID_BOOT_PROTOCOL_MOUSE     = 2;     {Section 4.3}

 {HID Request types}
 USB_HID_REQUEST_GET_REPORT      = $01;
 USB_HID_REQUEST_GET_IDLE        = $02;
 USB_HID_REQUEST_GET_PROTOCOL    = $03;   {Section 7.2}
 USB_HID_REQUEST_SET_REPORT      = $09;
 USB_HID_REQUEST_SET_IDLE        = $0A;
 USB_HID_REQUEST_SET_PROTOCOL    = $0B;   {Section 7.2}
 
 {HID Protocol types}
 USB_HID_PROTOCOL_BOOT           = 0;     {Section 7.2.5}
 USB_HID_PROTOCOL_REPORT         = 1;     {Section 7.2.5}
 
 {HID Report types}
 USB_HID_REPORT_INPUT            = 1;     {Section 7.2.1}
 USB_HID_REPORT_OUTPUT           = 2;     {Section 7.2.1}
 USB_HID_REPORT_FEATURE          = 3;     {Section 7.2.1}
 
 {HID Report IDs}
 USB_HID_REPORTID_NONE           = 0;     {Section 7.2.1}
 
 {HID Boot Protocol Button bits}
 USB_HID_BOOT_LEFT_BUTTON    = (1 shl 0);
 USB_HID_BOOT_RIGHT_BUTTON   = (1 shl 1);
 USB_HID_BOOT_MIDDLE_BUTTON  = (1 shl 2);
 USB_HID_BOOT_SIDE_BUTTON    = (1 shl 3);
 USB_HID_BOOT_EXTRA_BUTTON   = (1 shl 4);
 
 {HID Boot Protocol Report data}
 USB_HID_BOOT_REPORT_SIZE  = 3;            {Appendix B of HID Device Class Definition 1.11}
 USB_HID_BOOT_DATA_SIZE    = 8;            {Allocate more than the minimum to allow for extra data} 
 
{==============================================================================}
type
 {Mouse specific types}
 {Mouse Data}
 PMouseData = ^TMouseData;
 TMouseData = record
  Buttons:Word;
  OffsetX:SmallInt;
  OffsetY:SmallInt;
  OffsetWheel:SmallInt;
 end;
 
 {Mouse Buffer}
 PMouseBuffer = ^TMouseBuffer;
 TMouseBuffer = record
  Wait:TSemaphoreHandle;     {Buffer ready semaphore}
  Start:LongWord;            {Index of first buffer ready}
  Count:LongWord;            {Number of entries ready in buffer}
  Buffer:array[0..(MOUSE_BUFFER_SIZE - 1)] of TMouseData; 
 end;
 
 {Mouse Device}
 PMouseDevice = ^TMouseDevice;
 
 {Mouse Enumeration Callback}
 TMouseEnumerate = function(Mouse:PMouseDevice;Data:Pointer):LongWord;
 {Mouse Notification Callback}
 TMouseNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {Mouse Device Methods}
 TMouseDeviceRead = function(Mouse:PMouseDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; 
 TMouseDeviceControl = function(Mouse:PMouseDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
 
 TMouseDevice = record
  {Device Properties}
  Device:TDevice;                      {The Device entry for this Mouse}
  {Mouse Properties}
  MouseId:LongWord;                    {Unique Id of this Mouse in the Mouse table}
  MouseState:LongWord;                 {Mouse state (eg MOUSE_STATE_ATTACHED)}
  DeviceRead:TMouseDeviceRead;         {A Device specific DeviceRead method implementing a standard Mouse device interface (Or nil if the default method is suitable)}
  DeviceControl:TMouseDeviceControl;   {A Device specific DeviceControl method implementing a standard Mouse device interface (Or nil if the default method is suitable)}
  {Driver Properties}
  Lock:TMutexHandle;                   {Mouse lock}
  Buffer:TMouseBuffer;                 {Mouse input buffer}
  {Statistics Properties}
  ReceiveCount:LongWord;
  ReceiveErrors:LongWord;
  BufferOverruns:LongWord;
  {Internal Properties}                                                                        
  Prev:PMouseDevice;                   {Previous entry in Mouse table}
  Next:PMouseDevice;                   {Next entry in Mouse table}
 end;
 
{==============================================================================}
type
 {USB Mouse specific types}
 PUSBMouseDevice = ^TUSBMouseDevice;
 TUSBMouseDevice = record
  {Mouse Properties}
  Mouse:TMouseDevice;
  {USB Properties}
  HIDInterface:PUSBInterface;            {USB HID Mouse Interface}
  ReportRequest:PUSBRequest;             {USB request for mouse report data}
  ReportEndpoint:PUSBEndpointDescriptor; {USB Mouse Interrupt IN Endpoint}
  PendingCount:LongWord;                 {Number of USB requests pending for this mouse}
  WaiterThread:TThreadId;                {Thread waiting for pending requests to complete (for mouse detachment)}
 end;
  
{==============================================================================}
{var}
 {Mouse specific variables}

{==============================================================================}
{var}
 {USB Mouse specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure MouseInit;

{==============================================================================}
{Mouse Functions}
function MousePeek:LongWord;
function MouseRead(Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; 
function MouseReadEx(Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord; 

function MouseWrite(Buffer:Pointer;Size,Count:LongWord):LongWord; 

function MouseFlush:LongWord;

function MouseDeviceRead(Mouse:PMouseDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; 
function MouseDeviceControl(Mouse:PMouseDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;

function MouseDeviceSetState(Mouse:PMouseDevice;State:LongWord):LongWord;

function MouseDeviceCreate:PMouseDevice;
function MouseDeviceCreateEx(Size:LongWord):PMouseDevice;
function MouseDeviceDestroy(Mouse:PMouseDevice):LongWord;

function MouseDeviceRegister(Mouse:PMouseDevice):LongWord;
function MouseDeviceDeregister(Mouse:PMouseDevice):LongWord;

function MouseDeviceFind(MouseId:LongWord):PMouseDevice;
function MouseDeviceFindByName(const Name:String):PMouseDevice; inline;
function MouseDeviceFindByDescription(const Description:String):PMouseDevice; inline;
function MouseDeviceEnumerate(Callback:TMouseEnumerate;Data:Pointer):LongWord;

function MouseDeviceNotification(Mouse:PMouseDevice;Callback:TMouseNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL Console Functions}
function SysConsoleHideMouse(AUserData:Pointer):Boolean;
function SysConsoleShowMouse(X,Y:LongWord;AUserData:Pointer):Boolean;
function SysConsoleReadMouse(var X,Y,Buttons:LongWord;AUserData:Pointer):Boolean;

{==============================================================================}
{USB Mouse Functions}
function USBMouseDeviceRead(Mouse:PMouseDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; 
function USBMouseDeviceControl(Mouse:PMouseDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;

function USBMouseDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function USBMouseDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

procedure USBMouseReportWorker(Request:PUSBRequest); 
procedure USBMouseReportComplete(Request:PUSBRequest); 

{==============================================================================}
{Mouse Helper Functions}
function MouseGetCount:LongWord; inline;

function MouseDeviceCheck(Mouse:PMouseDevice):PMouseDevice;

function MouseDeviceTypeToString(MouseType:LongWord):String;
function MouseDeviceStateToString(MouseState:LongWord):String;

function MouseDeviceStateToNotification(State:LongWord):LongWord;

procedure MouseLog(Level:LongWord;Mouse:PMouseDevice;const AText:String);
procedure MouseLogInfo(Mouse:PMouseDevice;const AText:String);
procedure MouseLogError(Mouse:PMouseDevice;const AText:String);
procedure MouseLogDebug(Mouse:PMouseDevice;const AText:String);

{==============================================================================}
{USB Mouse Helper Functions}
function USBMouseDeviceSetProtocol(Mouse:PUSBMouseDevice;Protocol:Byte):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Mouse specific variables}
 MouseInitialized:Boolean;

 MouseTable:PMouseDevice;
 MouseTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 MouseTableCount:LongWord;

 MouseBuffer:PMouseBuffer;                             {Global mouse input buffer}
 MouseBufferLock:TMutexHandle = INVALID_HANDLE_VALUE;  {Global mouse buffer lock}
 
{==============================================================================}
{==============================================================================}
var
 {USB Mouse specific variables}
 USBMouseDriver:PUSBDriver;  {USB Mouse Driver interface (Set by MouseInit)}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure MouseInit;
{Initialize the mouse unit, device table and USB mouse driver}

{Note: Called only during system startup}
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if MouseInitialized then Exit;
 
 {Initialize Logging}
 MOUSE_LOG_ENABLED:=(MOUSE_DEFAULT_LOG_LEVEL <> MOUSE_LOG_LEVEL_NONE); 
 
 {Initialize Mouse Table}
 MouseTable:=nil;
 MouseTableLock:=CriticalSectionCreate; 
 MouseTableCount:=0;
 if MouseTableLock = INVALID_HANDLE_VALUE then
  begin
   if MOUSE_LOG_ENABLED then MouseLogError(nil,'Failed to create mouse table lock');
  end;
 
 {Initialize Mouse Buffer}
 MouseBuffer:=AllocMem(SizeOf(TMouseBuffer));
 MouseBufferLock:=INVALID_HANDLE_VALUE;
 if MouseBuffer = nil then
  begin
   if MOUSE_LOG_ENABLED then MouseLogError(nil,'Failed to allocate mouse buffer');
  end
 else
  begin
   {Create Semaphore}
   MouseBuffer.Wait:=SemaphoreCreate(0);
   if MouseBuffer.Wait = INVALID_HANDLE_VALUE then
    begin
     if MOUSE_LOG_ENABLED then MouseLogError(nil,'Failed to create mouse buffer semaphore');
    end;

   {Create Lock} 
   MouseBufferLock:=MutexCreate; 
   if MouseBufferLock = INVALID_HANDLE_VALUE then
    begin
     if MOUSE_LOG_ENABLED then MouseLogError(nil,'Failed to create mouse buffer lock');
    end;
  end;  
 
 {Create USB Mouse Driver}
 USBMouseDriver:=USBDriverCreate;
 if USBMouseDriver <> nil then
  begin
   {Update USB Mouse Driver}
   {Driver}
   USBMouseDriver.Driver.DriverName:=USBMOUSE_DRIVER_NAME; 
   {USB}
   USBMouseDriver.DriverBind:=USBMouseDriverBind;
   USBMouseDriver.DriverUnbind:=USBMouseDriverUnbind;
   
   {Register USB Mouse Driver}
   Status:=USBDriverRegister(USBMouseDriver); 
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'Mouse: Failed to register USB mouse driver: ' + USBStatusToString(Status));
    end;
  end
 else
  begin
   if MOUSE_LOG_ENABLED then MouseLogError(nil,'Failed to create USB mouse driver');
  end;
 
 {Setup Platform Console Handlers}
 ConsoleHideMouseHandler:=SysConsoleHideMouse;
 ConsoleShowMouseHandler:=SysConsoleShowMouse;
 ConsoleReadMouseHandler:=SysConsoleReadMouse;
 
 MouseInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Mouse Functions}
function MousePeek:LongWord;
{Peek at the global mouse buffer to see if any data packets are ready}
{Return: ERROR_SUCCESS if packets are ready, ERROR_NO_MORE_ITEMS if not or another error code on failure}
var
 Count:LongWord;
 Data:TMouseData;
begin
 {}
 Result:=MouseReadEx(@Data,SizeOf(TMouseData),MOUSE_FLAG_NON_BLOCK or MOUSE_FLAG_PEEK_BUFFER,Count);
end;

{==============================================================================}

function MouseRead(Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; 
{Read mouse data packets from the global mouse buffer}
{Buffer: Pointer to a buffer to copy the mouse data packets to}
{Size: The size of the buffer in bytes (Must be at least TMouseData or greater)}
{Count: The number of mouse data packets copied to the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=MouseReadEx(Buffer,Size,MOUSE_FLAG_NONE,Count);
end;

{==============================================================================}

function MouseReadEx(Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Read mouse data packets from the global mouse buffer}
{Buffer: Pointer to a buffer to copy the mouse data packets to}
{Size: The size of the buffer in bytes (Must be at least TMouseData or greater)}
{Flags: The flags for the behaviour of the read (eg MOUSE_FLAG_NON_BLOCK)}
{Count: The number of mouse data packets copied to the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Offset:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Size}
 if Size < SizeOf(TMouseData) then Exit;
 
 {$IFDEF MOUSE_DEBUG}
 if MOUSE_LOG_ENABLED then MouseLogDebug(nil,'Attempting to read ' + IntToStr(Size) + ' bytes from mouse');
 {$ENDIF}
 
 {Read to Buffer}
 Count:=0;
 Offset:=0;
 while Size >= SizeOf(TMouseData) do
  begin
   {Check Non Blocking}
   if ((Flags and MOUSE_FLAG_NON_BLOCK) <> 0) and (MouseBuffer.Count = 0) then
    begin
     if Count = 0 then Result:=ERROR_NO_MORE_ITEMS;
     Break;
    end;

   {Check Peek Buffer}
   if (Flags and MOUSE_FLAG_PEEK_BUFFER) <> 0 then
    begin
     {Acquire the Lock}
     if MutexLock(MouseBufferLock) = ERROR_SUCCESS then
      begin
       try
        if MouseBuffer.Count > 0 then
         begin
          {Copy Data}
          PMouseData(PtrUInt(Buffer) + Offset)^:=MouseBuffer.Buffer[MouseBuffer.Start];
          
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
        MutexUnlock(MouseBufferLock);
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
     {Wait for Mouse Data}
     if SemaphoreWait(MouseBuffer.Wait) = ERROR_SUCCESS then
      begin
       {Acquire the Lock}
       if MutexLock(MouseBufferLock) = ERROR_SUCCESS then
        begin
         try
          {Copy Data}
          PMouseData(PtrUInt(Buffer) + Offset)^:=MouseBuffer.Buffer[MouseBuffer.Start];
            
          {Update Start}
          MouseBuffer.Start:=(MouseBuffer.Start + 1) mod MOUSE_BUFFER_SIZE;
          
          {Update Count}
          Dec(MouseBuffer.Count);
    
          {Update Count}
          Inc(Count);
            
          {Upate Size and Offset}
          Dec(Size,SizeOf(TMouseData));
          Inc(Offset,SizeOf(TMouseData));
         finally
          {Release the Lock}
          MutexUnlock(MouseBufferLock);
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
  
 {$IFDEF MOUSE_DEBUG}
 if MOUSE_LOG_ENABLED then MouseLogDebug(nil,'Return count=' + IntToStr(Count));
 {$ENDIF}
end;

{==============================================================================}

function MouseWrite(Buffer:Pointer;Size,Count:LongWord):LongWord; 
{Write mouse data packets to the global mouse buffer}
{Buffer: Pointer to a buffer to copy the mouse data packets from}
{Size: The size of the buffer in bytes (Must be at least TMouseData or greater)}
{Count: The number of mouse data packets to copy from the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Offset:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Size}
 if Size < SizeOf(TMouseData) then Exit;
 
 {Check Count}
 if Count < 1 then Exit;
 
 {$IFDEF MOUSE_DEBUG}
 if MOUSE_LOG_ENABLED then MouseLogDebug(nil,'Attempting to write ' + IntToStr(Size) + ' bytes to mouse');
 {$ENDIF}

 {Write from Buffer}
 Offset:=0;
 while (Size >= SizeOf(TMouseData)) and (Count > 0) do
  begin
   {Acquire the Lock}
   if MutexLock(MouseBufferLock) = ERROR_SUCCESS then
    begin
     try
      {Check Buffer}
      if (MouseBuffer.Count < MOUSE_BUFFER_SIZE) then
       begin
        {Copy Data}
        MouseBuffer.Buffer[(MouseBuffer.Start + MouseBuffer.Count) mod MOUSE_BUFFER_SIZE]:=PMouseData(PtrUInt(Buffer) + Offset)^;
        
        {Update Count}
        Inc(MouseBuffer.Count);
        
        {Update Count}
        Dec(Count);
        
        {Upate Size and Offset}
        Dec(Size,SizeOf(TMouseData));
        Inc(Offset,SizeOf(TMouseData));
        
        {Signal Data Received}
        SemaphoreSignal(MouseBuffer.Wait); 
       end
      else
       begin
        Result:=ERROR_INSUFFICIENT_BUFFER;
        Exit;
       end;
     finally
      {Release the Lock}
      MutexUnlock(MouseBufferLock);
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
 
{==============================================================================}

function MouseFlush:LongWord;
{Flush the contents of the global mouse buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Acquire the Lock}
 if MutexLock(MouseBufferLock) = ERROR_SUCCESS then
  begin
   try
    while MouseBuffer.Count > 0 do
     begin
      {Wait for Data (Should not Block)}
      if SemaphoreWait(MouseBuffer.Wait) = ERROR_SUCCESS then
       begin
        {Update Start} 
        MouseBuffer.Start:=(MouseBuffer.Start + 1) mod MOUSE_BUFFER_SIZE;
        
        {Update Count}
        Dec(MouseBuffer.Count);
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
    MutexUnlock(MouseBufferLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
   Exit;
  end;
end;
 
{==============================================================================}

function MouseDeviceRead(Mouse:PMouseDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; 
{Read mouse data packets from the buffer of the specified mouse}
{Mouse: The mouse device to read from}
{Buffer: Pointer to a buffer to copy the mouse data packets to}
{Size: The size of the buffer in bytes (Must be at least TMouseData or greater)}
{Count: The number of mouse data packets copied to the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Offset:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;
 if Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Size}
 if Size < SizeOf(TMouseData) then Exit;
 
 {Check Method}
 if Assigned(Mouse.DeviceRead) then
  begin
   {Provided Method}
   Result:=Mouse.DeviceRead(Mouse,Buffer,Size,Count);
  end
 else
  begin 
   {Default Method}
   {Check Mouse Attached}
   if Mouse.MouseState <> MOUSE_STATE_ATTACHED then Exit;

   {$IFDEF MOUSE_DEBUG}
   if MOUSE_LOG_ENABLED then MouseLogDebug(Mouse,'Attempting to read ' + IntToStr(Size) + ' bytes from mouse');
   {$ENDIF}
   
   {Read to Buffer}
   Count:=0;
   Offset:=0;
   while Size >= SizeOf(TMouseData) do
    begin
     {Check Non Blocking}
     if ((Mouse.Device.DeviceFlags and MOUSE_FLAG_NON_BLOCK) <> 0) and (Mouse.Buffer.Count = 0) then
      begin
       if Count = 0 then Result:=ERROR_NO_MORE_ITEMS;
       Break;
      end;
    
     {Wait for Mouse Data}
     if SemaphoreWait(Mouse.Buffer.Wait) = ERROR_SUCCESS then
      begin
       {Acquire the Lock}
       if MutexLock(Mouse.Lock) = ERROR_SUCCESS then
        begin
         try
          {Copy Data}
          PMouseData(PtrUInt(Buffer) + Offset)^:=Mouse.Buffer.Buffer[Mouse.Buffer.Start];
          
          {Update Start}
          Mouse.Buffer.Start:=(Mouse.Buffer.Start + 1) mod MOUSE_BUFFER_SIZE;
        
          {Update Count}
          Dec(Mouse.Buffer.Count);
  
          {Update Count}
          Inc(Count);
          
          {Upate Size and Offset}
          Dec(Size,SizeOf(TMouseData));
          Inc(Offset,SizeOf(TMouseData));
         finally
          {Release the Lock}
          MutexUnlock(Mouse.Lock);
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
     
     {Return Result}
     Result:=ERROR_SUCCESS;
    end;
    
   {$IFDEF MOUSE_DEBUG}
   if MOUSE_LOG_ENABLED then MouseLogDebug(Mouse,'Return count=' + IntToStr(Count));
   {$ENDIF}
  end; 
end;
 
{==============================================================================}

function MouseDeviceControl(Mouse:PMouseDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
{Perform a control request on the specified mouse device}
{Mouse: The mouse device to control}
{Request: The request code for the operation (eg MOUSE_CONTROL_GET_FLAG)}
{Argument1: The first argument for the operation (Dependant on request code)}
{Argument2: The second argument for the operation (Dependant on request code)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;
 if Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 if Assigned(Mouse.DeviceControl) then
  begin
   {Provided Method}
   Result:=Mouse.DeviceControl(Mouse,Request,Argument1,Argument2);
  end
 else
  begin 
   {Default Method}
   {Check Mouse Attached}
   if Mouse.MouseState <> MOUSE_STATE_ATTACHED then Exit;

   {Acquire the Lock}
   if MutexLock(Mouse.Lock) = ERROR_SUCCESS then
    begin
     try
      case Request of
       MOUSE_CONTROL_GET_FLAG:begin
         {Get Flag}
         LongBool(Argument2):=False;
         if (Mouse.Device.DeviceFlags and Argument1) <> 0 then
          begin
           LongBool(Argument2):=True;
           
           {Return Result}
           Result:=ERROR_SUCCESS;
          end;
        end;
       MOUSE_CONTROL_SET_FLAG:begin 
         {Set Flag}
         if (Argument1 and not(MOUSE_FLAG_MASK)) = 0 then
          begin
           Mouse.Device.DeviceFlags:=(Mouse.Device.DeviceFlags or Argument1);
         
           {Return Result}
           Result:=ERROR_SUCCESS;
          end; 
        end;
       MOUSE_CONTROL_CLEAR_FLAG:begin 
         {Clear Flag}
         if (Argument1 and not(MOUSE_FLAG_MASK)) = 0 then
          begin
           Mouse.Device.DeviceFlags:=(Mouse.Device.DeviceFlags and not(Argument1));
         
           {Return Result}
           Result:=ERROR_SUCCESS;
          end; 
        end;
       MOUSE_CONTROL_FLUSH_BUFFER:begin
         {Flush Buffer}
         while Mouse.Buffer.Count > 0 do 
          begin
           {Wait for Data (Should not Block)}
           if SemaphoreWait(Mouse.Buffer.Wait) = ERROR_SUCCESS then
            begin
             {Update Start}
             Mouse.Buffer.Start:=(Mouse.Buffer.Start + 1) mod MOUSE_BUFFER_SIZE;
             
             {Update Count}
             Dec(Mouse.Buffer.Count);
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
      end;
     finally
      {Release the Lock}
      MutexUnlock(Mouse.Lock);
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

function MouseDeviceSetState(Mouse:PMouseDevice;State:LongWord):LongWord;
{Set the state of the specified mouse and send a notification}
{Mouse: The mouse to set the state for}
{State: The new state to set and notify}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;
 if Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check State}
 if State > MOUSE_STATE_ATTACHED then Exit;
 
 {Check State}
 if Mouse.MouseState = State then
  begin
   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Mouse.Lock) = ERROR_SUCCESS then
    begin
     try 
      {Set State}
      Mouse.MouseState:=State;
  
      {Notify State}
      NotifierNotify(@Mouse.Device,MouseDeviceStateToNotification(State));

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Mouse.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;  
end;

{==============================================================================}

function MouseDeviceCreate:PMouseDevice;
{Create a new Mouse device entry}
{Return: Pointer to new Mouse device entry or nil if mouse could not be created}
begin
 {}
 Result:=MouseDeviceCreateEx(SizeOf(TMouseDevice));
end;

{==============================================================================}

function MouseDeviceCreateEx(Size:LongWord):PMouseDevice;
{Create a new Mouse device entry}
{Size: Size in bytes to allocate for new mouse (Including the mouse device entry)}
{Return: Pointer to new Mouse device entry or nil if mouse could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TMouseDevice) then Exit;
 
 {Create Mouse}
 Result:=PMouseDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=MOUSE_TYPE_NONE;
 Result.Device.DeviceFlags:=MOUSE_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Mouse}
 Result.MouseId:=DEVICE_ID_ANY;
 Result.MouseState:=MOUSE_STATE_DETACHED;
 Result.DeviceRead:=nil;
 Result.DeviceControl:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Buffer.Wait:=INVALID_HANDLE_VALUE;
 
 {Check Defaults}
 if MOUSE_SWAP_BUTTONS_DEFAULT then Result.Device.DeviceFlags:=Result.Device.DeviceFlags or MOUSE_FLAG_SWAP_BUTTONS;
 
 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if MOUSE_LOG_ENABLED then MouseLogError(nil,'Failed to create lock for mouse');
   MouseDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
 
 {Create Buffer Semaphore}
 Result.Buffer.Wait:=SemaphoreCreate(0);
 if Result.Buffer.Wait = INVALID_HANDLE_VALUE then
  begin
   if MOUSE_LOG_ENABLED then MouseLogError(nil,'Failed to create buffer semaphore for mouse');
   MouseDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function MouseDeviceDestroy(Mouse:PMouseDevice):LongWord;
{Destroy an existing Mouse device entry}
{Mouse: The mouse device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;
 if Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Mouse}
 Result:=ERROR_IN_USE;
 if MouseDeviceCheck(Mouse) = Mouse then Exit;

 {Check State}
 if Mouse.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Buffer Semaphore}
 if Mouse.Buffer.Wait <> INVALID_HANDLE_VALUE then
  begin
   SemaphoreDestroy(Mouse.Buffer.Wait);
  end;
  
 {Destroy Lock}
 if Mouse.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Mouse.Lock);
  end;
 
 {Destroy Mouse} 
 Result:=DeviceDestroy(@Mouse.Device);
end;

{==============================================================================}

function MouseDeviceRegister(Mouse:PMouseDevice):LongWord;
{Register a new Mouse device in the Mouse table}
{Mouse: The mouse device to register}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 MouseId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;
 if Mouse.MouseId <> DEVICE_ID_ANY then Exit;
 if Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Mouse}
 Result:=ERROR_ALREADY_EXISTS;
 if MouseDeviceCheck(Mouse) = Mouse then Exit;
 
 {Check State}
 if Mouse.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert Mouse}
 if CriticalSectionLock(MouseTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Mouse}
    MouseId:=0;
    while MouseDeviceFind(MouseId) <> nil do
     begin
      Inc(MouseId);
     end;
    Mouse.MouseId:=MouseId;
    
    {Update Device}
    Mouse.Device.DeviceName:=MOUSE_NAME_PREFIX + IntToStr(Mouse.MouseId); 
    Mouse.Device.DeviceClass:=DEVICE_CLASS_MOUSE;
    
    {Register Device}
    Result:=DeviceRegister(@Mouse.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Mouse.MouseId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link Mouse}
    if MouseTable = nil then
     begin
      MouseTable:=Mouse;
     end
    else
     begin
      Mouse.Next:=MouseTable;
      MouseTable.Prev:=Mouse;
      MouseTable:=Mouse;
     end;
 
    {Increment Count}
    Inc(MouseTableCount);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(MouseTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function MouseDeviceDeregister(Mouse:PMouseDevice):LongWord;
{Deregister a Mouse device from the Mouse table}
{Mouse: The mouse device to deregister}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Prev:PMouseDevice;
 Next:PMouseDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;
 if Mouse.MouseId = DEVICE_ID_ANY then Exit;
 if Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Mouse}
 Result:=ERROR_NOT_FOUND;
 if MouseDeviceCheck(Mouse) <> Mouse then Exit;
 
 {Check State}
 if Mouse.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove Mouse}
 if CriticalSectionLock(MouseTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Mouse.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink Mouse}
    Prev:=Mouse.Prev;
    Next:=Mouse.Next;
    if Prev = nil then
     begin
      MouseTable:=Next;
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
    Dec(MouseTableCount);
 
    {Update Mouse}
    Mouse.MouseId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(MouseTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function MouseDeviceFind(MouseId:LongWord):PMouseDevice;
{Find a mouse device by ID in the mouse table}
{MouseId: The ID number of the mouse to find}
{Return: Pointer to mouse device entry or nil if not found}
var
 Mouse:PMouseDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if MouseId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(MouseTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Mouse}
    Mouse:=MouseTable;
    while Mouse <> nil do
     begin
      {Check State}
      if Mouse.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Mouse.MouseId = MouseId then
         begin
          Result:=Mouse;
          Exit;
         end;
       end;
       
      {Get Next}
      Mouse:=Mouse.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(MouseTableLock);
   end;
  end;
end;
       
{==============================================================================}
       
function MouseDeviceFindByName(const Name:String):PMouseDevice; inline;
{Find a mouse device by name in the mouse table}
{Name: The name of the mouse to find (eg Mouse0)}
{Return: Pointer to mouse device entry or nil if not found}
begin
 {}
 Result:=PMouseDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function MouseDeviceFindByDescription(const Description:String):PMouseDevice; inline;
{Find a mouse device by description in the mouse table}
{Description: The description of the mouse to find (eg USB HID Mouse)}
{Return: Pointer to mouse device entry or nil if not found}
begin
 {}
 Result:=PMouseDevice(DeviceFindByDescription(Description));
end;
       
{==============================================================================}

function MouseDeviceEnumerate(Callback:TMouseEnumerate;Data:Pointer):LongWord;
{Enumerate all mouse devices in the mouse table}
{Callback: The callback function to call for each mouse in the table}
{Data: A private data pointer to pass to callback for each mouse in the table}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Mouse:PMouseDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(MouseTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Mouse}
    Mouse:=MouseTable;
    while Mouse <> nil do
     begin
      {Check State}
      if Mouse.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Mouse,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Mouse:=Mouse.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(MouseTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function MouseDeviceNotification(Mouse:PMouseDevice;Callback:TMouseNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
{Register a notification for mouse device changes}
{Mouse: The mouse device to notify changes for (Optional, pass nil for all mice)}
{Callback: The function to call when a notification event occurs}
{Data: A private data pointer to pass to callback when a notification event occurs}
{Notification: The events to register for notification of (eg DEVICE_NOTIFICATION_REGISTER)}
{Flags: The flags to control the notification (eg NOTIFIER_FLAG_WORKER)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_MOUSE,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check Mouse}
   if Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Mouse.Device,DEVICE_CLASS_MOUSE,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{RTL Console Functions}
function SysConsoleHideMouse(AUserData:Pointer):Boolean;
{Handler for Platform ConsoleHideMouse function}
begin
 {}
 Result:=True;
end;

{==============================================================================}

function SysConsoleShowMouse(X,Y:LongWord;AUserData:Pointer):Boolean;
{Handler for Platform ConsoleShowMouse function}
begin
 {}
 Result:=True;
end;

{==============================================================================}

function SysConsoleReadMouse(var X,Y,Buttons:LongWord;AUserData:Pointer):Boolean;
{Handler for Platform ConsoleReadMouse function}
var
 Count:LongWord;
 Data:TMouseData;
begin
 {}
 Result:=True;
 
 if MouseRead(@Data,SizeOf(TMouseData),Count) = ERROR_SUCCESS then
  begin
   X:=Data.OffsetX;
   Y:=Data.OffsetY;
   Buttons:=Data.Buttons;
  end
 else
  begin 
   X:=0;
   Y:=0;
   Buttons:=0;
  end; 
end;

{==============================================================================}
{==============================================================================}
{USB Mouse Functions}
function USBMouseDeviceRead(Mouse:PMouseDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; 
{Implementation of MouseDeviceRead API for USB Mouse}
var
 Offset:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;
 if Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Size}
 if Size < SizeOf(TMouseData) then Exit;
 
 {Check Mouse Attached}
 if Mouse.MouseState <> MOUSE_STATE_ATTACHED then Exit;
 
 {$IFDEF MOUSE_DEBUG}
 if MOUSE_LOG_ENABLED then MouseLogDebug(Mouse,'Attempting to read ' + IntToStr(Size) + ' bytes from mouse');
 {$ENDIF}
 
 {Read to Buffer}
 Count:=0;
 Offset:=0;
 while Size >= SizeOf(TMouseData) do
  begin
   {Check Non Blocking}
   if ((Mouse.Device.DeviceFlags and MOUSE_FLAG_NON_BLOCK) <> 0) and (Mouse.Buffer.Count = 0) then
    begin
     if Count = 0 then Result:=ERROR_NO_MORE_ITEMS;
     Break;
    end;

   {Wait for Mouse Data}
   if SemaphoreWait(Mouse.Buffer.Wait) = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(Mouse.Lock) = ERROR_SUCCESS then
      begin
       try
        {Copy Data}
        PMouseData(PtrUInt(Buffer) + Offset)^:=Mouse.Buffer.Buffer[Mouse.Buffer.Start];
          
        {Update Start}
        Mouse.Buffer.Start:=(Mouse.Buffer.Start + 1) mod MOUSE_BUFFER_SIZE;
        
        {Update Count}
        Dec(Mouse.Buffer.Count);
  
        {Update Count}
        Inc(Count);
          
        {Upate Size and Offset}
        Dec(Size,SizeOf(TMouseData));
        Inc(Offset,SizeOf(TMouseData));
       finally
        {Release the Lock}
        MutexUnlock(Mouse.Lock);
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
   
   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
  
 {$IFDEF MOUSE_DEBUG}
 if MOUSE_LOG_ENABLED then MouseLogDebug(Mouse,'Return count=' + IntToStr(Count));
 {$ENDIF}
end;
 
{==============================================================================}

function USBMouseDeviceControl(Mouse:PMouseDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
{Implementation of MouseDeviceControl API for USB Mouse}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;
 if Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Check Mouse Attached}
 if Mouse.MouseState <> MOUSE_STATE_ATTACHED then Exit;
 
 {Acquire the Lock}
 if MutexLock(Mouse.Lock) = ERROR_SUCCESS then
  begin
   try
    case Request of
     MOUSE_CONTROL_GET_FLAG:begin
       {Get Flag}
       LongBool(Argument2):=False;
       if (Mouse.Device.DeviceFlags and Argument1) <> 0 then
        begin
         LongBool(Argument2):=True;
         
         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     MOUSE_CONTROL_SET_FLAG:begin 
       {Set Flag}
       if (Argument1 and not(MOUSE_FLAG_MASK)) = 0 then
        begin
         Mouse.Device.DeviceFlags:=(Mouse.Device.DeviceFlags or Argument1);
       
         {Return Result}
         Result:=ERROR_SUCCESS;
        end; 
      end;
     MOUSE_CONTROL_CLEAR_FLAG:begin 
       {Clear Flag}
       if (Argument1 and not(MOUSE_FLAG_MASK)) = 0 then
        begin
         Mouse.Device.DeviceFlags:=(Mouse.Device.DeviceFlags and not(Argument1));
       
         {Return Result}
         Result:=ERROR_SUCCESS;
        end; 
      end;
     MOUSE_CONTROL_FLUSH_BUFFER:begin
       {Flush Buffer}
       while Mouse.Buffer.Count > 0 do 
        begin
         {Wait for Data (Should not Block)}
         if SemaphoreWait(Mouse.Buffer.Wait) = ERROR_SUCCESS then
          begin
           {Update Start}
           Mouse.Buffer.Start:=(Mouse.Buffer.Start + 1) mod MOUSE_BUFFER_SIZE;
           
           {Update Count}
           Dec(Mouse.Buffer.Count);
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
    end;
   finally
    {Release the Lock}
    MutexUnlock(Mouse.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
   Exit;
  end;
end;
 
{==============================================================================}
 
function USBMouseDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the Mouse driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
var
 Status:LongWord;
 Interval:LongWord;
 Mouse:PUSBMouseDevice;
 ReportEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
                        
 {$IFDEF USB_DEBUG}                       
 if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Attempting to bind USB device (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}
 
 {Check Interface (Bind to interface only)}
 if Interrface = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check for Mouse (Must be interface specific)}
 if Device.Descriptor.bDeviceClass <> USB_CLASS_CODE_INTERFACE_SPECIFIC then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;   
  end;
  
 {Check Interface (Must be HID boot protocol mouse)}
 if (Interrface.Descriptor.bInterfaceClass <> USB_CLASS_CODE_HID) or (Interrface.Descriptor.bInterfaceSubClass <> USB_HID_SUBCLASS_BOOT) or (Interrface.Descriptor.bInterfaceProtocol <> USB_HID_BOOT_PROTOCOL_MOUSE) then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;   
  end;
 
 {Check Endpoint (Must be IN interrupt)}
 ReportEndpoint:=USBDeviceFindEndpointByType(Device,Interrface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT);
 if ReportEndpoint = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Create Mouse}
 Mouse:=PUSBMouseDevice(MouseDeviceCreateEx(SizeOf(TUSBMouseDevice)));
 if Mouse = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Mouse: Failed to create new mouse device');
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Update Mouse} 
 {Device}
 Mouse.Mouse.Device.DeviceBus:=DEVICE_BUS_USB;
 Mouse.Mouse.Device.DeviceType:=MOUSE_TYPE_USB;
 Mouse.Mouse.Device.DeviceFlags:=Mouse.Mouse.Device.DeviceFlags; {Don't override defaults (was MOUSE_FLAG_NONE)}
 Mouse.Mouse.Device.DeviceData:=Device;
 Mouse.Mouse.Device.DeviceDescription:=USBMOUSE_MOUSE_DESCRIPTION;
 {Mouse}
 Mouse.Mouse.MouseState:=MOUSE_STATE_ATTACHING;
 Mouse.Mouse.DeviceRead:=USBMouseDeviceRead;
 Mouse.Mouse.DeviceControl:=USBMouseDeviceControl;
 {Driver}
 {USB}
 Mouse.HIDInterface:=Interrface;
 Mouse.ReportEndpoint:=ReportEndpoint;
 Mouse.WaiterThread:=INVALID_HANDLE_VALUE;
 
 {Allocate Report Request}
 Mouse.ReportRequest:=USBRequestAllocate(Device,ReportEndpoint,USBMouseReportComplete,USB_HID_BOOT_DATA_SIZE,Mouse);
 if Mouse.ReportRequest = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Mouse: Failed to allocate USB report request for mouse');

   {Destroy Mouse}
   MouseDeviceDestroy(@Mouse.Mouse);

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Register Mouse} 
 if MouseDeviceRegister(@Mouse.Mouse) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Mouse: Failed to register new mouse device');
   
   {Release Report Request}
   USBRequestRelease(Mouse.ReportRequest);
   
   {Destroy Mouse}
   MouseDeviceDestroy(@Mouse.Mouse);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Enabling HID boot protocol');
 {$ENDIF}

 {Set Boot Protocol}
 Status:=USBMouseDeviceSetProtocol(Mouse,USB_HID_PROTOCOL_BOOT);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Mouse: Failed to enable HID boot protocol: ' + USBStatusToString(Status));

   {Release Report Request}
   USBRequestRelease(Mouse.ReportRequest);
   
   {Deregister Mouse}
   MouseDeviceDeregister(@Mouse.Mouse);
   
   {Destroy Mouse}
   MouseDeviceDestroy(@Mouse.Mouse);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
  
 {Check Endpoint Interval}
 if USB_MOUSE_POLLING_INTERVAL > 0 then
  begin
   {Check Device Speed}
   if Device.Speed = USB_SPEED_HIGH then
    begin
     {Get Interval}
     Interval:=FirstBitSet(USB_MOUSE_POLLING_INTERVAL * USB_UFRAMES_PER_MS) + 1;
     
     {Ensure no less than Interval} {Milliseconds = (1 shl (bInterval - 1)) div USB_UFRAMES_PER_MS}
     if ReportEndpoint.bInterval < Interval then ReportEndpoint.bInterval:=Interval;
    end
   else
    begin
     {Ensure no less than USB_MOUSE_POLLING_INTERVAL} {Milliseconds = bInterval div USB_FRAMES_PER_MS}
     if ReportEndpoint.bInterval < USB_MOUSE_POLLING_INTERVAL then ReportEndpoint.bInterval:=USB_MOUSE_POLLING_INTERVAL;
    end;  
  end;  
  
 {Update Interface}
 Interrface.DriverData:=Mouse;
 
 {Update Pending}
 Inc(Mouse.PendingCount);
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Submitting report request');
 {$ENDIF}
 
 {Submit Request}
 Status:=USBRequestSubmit(Mouse.ReportRequest);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Mouse: Failed to submit report request: ' + USBStatusToString(Status));
   
   {Update Pending}
   Dec(Mouse.PendingCount);
   
   {Release Report Request}
   USBRequestRelease(Mouse.ReportRequest);
   
   {Deregister Mouse}
   MouseDeviceDeregister(@Mouse.Mouse);
   
   {Destroy Mouse}
   MouseDeviceDestroy(@Mouse.Mouse);
   
   {Return Result}
   Result:=Status;
   Exit;
  end;  
 
 {Set State to Attached}
 if MouseDeviceSetState(@Mouse.Mouse,MOUSE_STATE_ATTACHED) <> ERROR_SUCCESS then Exit;
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;
 
{==============================================================================}
 
function USBMouseDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the Mouse driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Message:TMessage;
 Mouse:PUSBMouseDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then Exit;
 
 {Check Driver}
 if Interrface.Driver <> USBMouseDriver then Exit;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Unbinding (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}
 
 {Get Mouse}
 Mouse:=PUSBMouseDevice(Interrface.DriverData);
 if Mouse = nil then Exit;
 if Mouse.Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Set State to Detaching}
 Result:=USB_STATUS_OPERATION_FAILED;
 if MouseDeviceSetState(@Mouse.Mouse,MOUSE_STATE_DETACHING) <> ERROR_SUCCESS then Exit;

 {Acquire the Lock}
 if MutexLock(Mouse.Mouse.Lock) <> ERROR_SUCCESS then Exit;
 
 {Cancel Report Request}
 USBRequestCancel(Mouse.ReportRequest);
 
 {Check Pending}
 if Mouse.PendingCount <> 0 then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Waiting for ' + IntToStr(Mouse.PendingCount) + ' pending requests to complete');
   {$ENDIF}
  
   {Wait for Pending}
   
   {Setup Waiter}
   Mouse.WaiterThread:=GetCurrentThreadId; 
   
   {Release the Lock}
   MutexUnlock(Mouse.Mouse.Lock);
   
   {Wait for Message}
   ThreadReceiveMessage(Message); 
  end
 else
  begin
   {Release the Lock}
   MutexUnlock(Mouse.Mouse.Lock);
  end;  
 
 {Set State to Detached}
 if MouseDeviceSetState(@Mouse.Mouse,MOUSE_STATE_DETACHED) <> ERROR_SUCCESS then Exit;
 
 {Update Interface}
 Interrface.DriverData:=nil; 

 {Release Report Request}
 USBRequestRelease(Mouse.ReportRequest);

 {Deregister Mouse}
 if MouseDeviceDeregister(@Mouse.Mouse) <> ERROR_SUCCESS then Exit;
 
 {Destroy Mouse}
 MouseDeviceDestroy(@Mouse.Mouse);
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;
 
{==============================================================================}

procedure USBMouseReportWorker(Request:PUSBRequest); 
{Called (by a Worker thread) to process a completed USB request from a USB mouse IN interrupt endpoint}
{Request: The USB request which has completed}
var
 Buffer:Pointer;
 Data:PMouseData;
 Status:LongWord;
 Message:TMessage;
 Mouse:PUSBMouseDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Mouse}
 Mouse:=PUSBMouseDevice(Request.DriverData);
 if Mouse <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Mouse.Mouse.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Mouse.Mouse.ReceiveCount); 
      
      {Check State}
      if Mouse.Mouse.MouseState = MOUSE_STATE_DETACHING then
       begin
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Mouse: Detachment pending, setting report request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}
        
        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;
 
      {Check Result}
      if (Request.Status = USB_STATUS_SUCCESS) and (Request.ActualSize >= USB_HID_BOOT_REPORT_SIZE) then  
       begin
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Mouse: Report received'); 
        {$ENDIF}
        
        {A report was received from the USB mouse}
        Buffer:=Request.Data;
     
        {Check Flags}
        if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_DIRECT_READ) = 0 then
         begin
          {Global Buffer}
          {Acquire the Lock}
          if MutexLock(MouseBufferLock) = ERROR_SUCCESS then
           begin
            try
             {Check Buffer}
             if (MouseBuffer.Count < MOUSE_BUFFER_SIZE) then
              begin
               Data:=@MouseBuffer.Buffer[(MouseBuffer.Start + MouseBuffer.Count) mod MOUSE_BUFFER_SIZE];
               if Data <> nil then
                begin
                 {Byte 0 is the Mouse buttons}
                 Data.Buttons:=0;
                 if (PByte(Buffer)^ and USB_HID_BOOT_LEFT_BUTTON) <> 0 then
                  begin
                   {Check Flags}
                   if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
                    begin
                     Data.Buttons:=Data.Buttons or MOUSE_LEFT_BUTTON;
                    end
                   else
                    begin
                     Data.Buttons:=Data.Buttons or MOUSE_RIGHT_BUTTON;
                    end;
                  end; 
                 if (PByte(Buffer)^ and USB_HID_BOOT_RIGHT_BUTTON) <> 0 then
                  begin
                   {Check Flags}
                   if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
                    begin
                     Data.Buttons:=Data.Buttons or MOUSE_RIGHT_BUTTON;
                    end
                   else
                    begin
                     Data.Buttons:=Data.Buttons or MOUSE_LEFT_BUTTON;
                    end;
                  end; 
                 if (PByte(Buffer)^ and USB_HID_BOOT_MIDDLE_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_MIDDLE_BUTTON;
                 if (PByte(Buffer)^ and USB_HID_BOOT_SIDE_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_SIDE_BUTTON;
                 if (PByte(Buffer)^ and USB_HID_BOOT_EXTRA_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_EXTRA_BUTTON;
         
                 {Byte 1 is the Mouse X offset}
                 Data.OffsetX:=PShortInt(PtrUInt(Buffer) + 1)^;
         
                 {Byte 2 is the Mouse Y offset}
                 Data.OffsetY:=PShortInt(PtrUInt(Buffer) + 2)^;
       
                 {Byte 3 is the Mouse Wheel offset}
                 Data.OffsetWheel:=PShortInt(PtrUInt(Buffer) + 3)^;
            
                 {Update Count}
                 Inc(MouseBuffer.Count);
            
                 {Signal Data Received}
                 SemaphoreSignal(MouseBuffer.Wait);
                end;
              end
             else
              begin
               if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Buffer overflow, report discarded');
               
               {Update Statistics}
               Inc(Mouse.Mouse.BufferOverruns); 
              end;            
            finally
             {Release the Lock}
             MutexUnlock(MouseBufferLock);
            end;
           end
          else
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Failed to acquire lock on buffer');
           end;
         end
        else
         begin              
          {Direct Buffer}
          {Check Buffer}
          if (Mouse.Mouse.Buffer.Count < MOUSE_BUFFER_SIZE) then
           begin
            Data:=@Mouse.Mouse.Buffer.Buffer[(Mouse.Mouse.Buffer.Start + Mouse.Mouse.Buffer.Count) mod MOUSE_BUFFER_SIZE];
            if Data <> nil then
             begin
              {Byte 0 is the Mouse buttons}
              Data.Buttons:=0;
              if (PByte(Buffer)^ and USB_HID_BOOT_LEFT_BUTTON) <> 0 then
               begin
                {Check Flags}
                if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
                 begin
                  Data.Buttons:=Data.Buttons or MOUSE_LEFT_BUTTON;
                 end
                else
                 begin
                  Data.Buttons:=Data.Buttons or MOUSE_RIGHT_BUTTON;
                 end;
               end; 
              if (PByte(Buffer)^ and USB_HID_BOOT_RIGHT_BUTTON) <> 0 then
               begin
                {Check Flags}
                if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
                 begin
                  Data.Buttons:=Data.Buttons or MOUSE_RIGHT_BUTTON;
                 end
                else
                 begin
                  Data.Buttons:=Data.Buttons or MOUSE_LEFT_BUTTON;
                 end;
               end; 
              if (PByte(Buffer)^ and USB_HID_BOOT_MIDDLE_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_MIDDLE_BUTTON;
              if (PByte(Buffer)^ and USB_HID_BOOT_SIDE_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_SIDE_BUTTON;
              if (PByte(Buffer)^ and USB_HID_BOOT_EXTRA_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_EXTRA_BUTTON;
         
              {Byte 1 is the Mouse X offset}
              Data.OffsetX:=PShortInt(PtrUInt(Buffer) + 1)^;
         
              {Byte 2 is the Mouse Y offset}
              Data.OffsetY:=PShortInt(PtrUInt(Buffer) + 2)^;
       
              {Byte 3 is the Mouse Wheel offset}
              Data.OffsetWheel:=PShortInt(PtrUInt(Buffer) + 3)^;
            
              {Update Count}
              Inc(Mouse.Mouse.Buffer.Count);
            
              {Signal Data Received}
              SemaphoreSignal(Mouse.Mouse.Buffer.Wait);
             end; 
           end
          else
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Buffer overflow, report discarded'); 

            {Update Statistics}
            Inc(Mouse.Mouse.BufferOverruns); 
           end;                           
         end;
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Failed report request (Status=' + USBStatusToString(Request.Status) + ', ActualSize=' + IntToStr(Request.ActualSize) + ')'); 
        
        {Update Statistics}
        Inc(Mouse.Mouse.ReceiveErrors); 
       end;       

      {Update Pending}
      Dec(Mouse.PendingCount); 
       
      {Check State}
      if Mouse.Mouse.MouseState = MOUSE_STATE_DETACHING then
       begin
        {Check Pending}
        if Mouse.PendingCount = 0 then
         begin
          {Check Waiter}
          if Mouse.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IFDEF USB_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Mouse: Detachment pending, sending message to waiter thread (Thread=' + IntToHex(Mouse.WaiterThread,8) + ')');
            {$ENDIF}
            
            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Mouse.WaiterThread,Message);
            Mouse.WaiterThread:=INVALID_HANDLE_VALUE;
           end; 
         end;
       end
      else
       begin      
        {Update Pending}
        Inc(Mouse.PendingCount);
      
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Mouse: Resubmitting report request');
        {$ENDIF}

        {Resubmit Request}
        Status:=USBRequestSubmit(Request);
        if Status <> USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Failed to resubmit report request: ' + USBStatusToString(Status));
   
          {Update Pending}
          Dec(Mouse.PendingCount);
         end;
       end;  
     finally
      {Release the Lock}
      MutexUnlock(Mouse.Mouse.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Report request invalid');
  end;    
end;
 
{==============================================================================}
 
procedure USBMouseReportComplete(Request:PUSBRequest);
{Called when a USB request from a USB mouse IN interrupt endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 WorkerSchedule(0,TWorkerTask(USBMouseReportWorker),Request,nil)
end;
 
{==============================================================================}
{==============================================================================}
{Mouse Helper Functions}
function MouseGetCount:LongWord; inline;
{Get the current mouse count}
begin
 {}
 Result:=MouseTableCount;
end;

{==============================================================================}

function MouseDeviceCheck(Mouse:PMouseDevice):PMouseDevice;
{Check if the supplied Mouse is in the mouse table}
var
 Current:PMouseDevice;
begin
 {}
 Result:=nil;
 
 {Check Mouse}
 if Mouse = nil then Exit;
 if Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(MouseTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Mouse}
    Current:=MouseTable;
    while Current <> nil do
     begin
      {Check Mouse}
      if Current = Mouse then
       begin
        Result:=Mouse;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(MouseTableLock);
   end;
  end;
end;

{==============================================================================}

function MouseDeviceTypeToString(MouseType:LongWord):String;
begin
 {}
 Result:='MOUSE_TYPE_UNKNOWN';
 
 if MouseType <= MOUSE_TYPE_MAX then
  begin
   Result:=MOUSE_TYPE_NAMES[MouseType];
  end;
end;

{==============================================================================}

function MouseDeviceStateToString(MouseState:LongWord):String;
begin
 {}
 Result:='MOUSE_STATE_UNKNOWN';
 
 if MouseState <= MOUSE_STATE_MAX then
  begin
   Result:=MOUSE_STATE_NAMES[MouseState];
  end;
end;

{==============================================================================}

function MouseDeviceStateToNotification(State:LongWord):LongWord;
{Convert a Mouse state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;
 
 {Check State}
 case State of
  MOUSE_STATE_DETACHED:Result:=DEVICE_NOTIFICATION_DETACH;
  MOUSE_STATE_DETACHING:Result:=DEVICE_NOTIFICATION_DETACHING;
  MOUSE_STATE_ATTACHING:Result:=DEVICE_NOTIFICATION_ATTACHING;
  MOUSE_STATE_ATTACHED:Result:=DEVICE_NOTIFICATION_ATTACH;
 end;
end;

{==============================================================================}

procedure MouseLog(Level:LongWord;Mouse:PMouseDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < MOUSE_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = MOUSE_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = MOUSE_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'Mouse: ';
 
 {Check Mouse}
 if Mouse <> nil then
  begin
   WorkBuffer:=WorkBuffer + MOUSE_NAME_PREFIX + IntToStr(Mouse.MouseId) + ': ';
  end;
  
 {Output Logging} 
 LoggingOutputEx(LOGGING_FACILITY_MOUSE,LogLevelToLoggingSeverity(Level),'Mouse',WorkBuffer + AText);
end;

{==============================================================================}

procedure MouseLogInfo(Mouse:PMouseDevice;const AText:String);
begin
 {}
 MouseLog(MOUSE_LOG_LEVEL_INFO,Mouse,AText);
end;

{==============================================================================}

procedure MouseLogError(Mouse:PMouseDevice;const AText:String);
begin
 {}
 MouseLog(MOUSE_LOG_LEVEL_ERROR,Mouse,AText);
end;

{==============================================================================}

procedure MouseLogDebug(Mouse:PMouseDevice;const AText:String);
begin
 {}
 MouseLog(MOUSE_LOG_LEVEL_DEBUG,Mouse,AText);
end;

{==============================================================================}
{==============================================================================}
{USB Mouse Helper Functions}
function USBMouseDeviceSetProtocol(Mouse:PUSBMouseDevice;Protocol:Byte):LongWord;
{Set the report protocol for a USB mouse device}
{Mouse: The USB mouse device to set the report protocol for}
{Protocol: The report protocol to set (eg USB_HID_PROTOCOL_BOOT)}
{Return: USB_STATUS_SUCCESS if completed or another USB error code on failure}
var
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Mouse}
 if Mouse = nil then Exit;
 
 {Check Interface}
 if Mouse.HIDInterface = nil then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Mouse.Mouse.Device.DeviceData);
 if Device = nil then Exit;
 
 {Set Protocol}
 Result:=USBControlRequest(Device,nil,USB_HID_REQUEST_SET_PROTOCOL,USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,Protocol,Mouse.HIDInterface.Descriptor.bInterfaceNumber,nil,0);
end;

{==============================================================================}
{==============================================================================}

initialization
 MouseInit;

{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
