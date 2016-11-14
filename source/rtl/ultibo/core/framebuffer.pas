{
Ultibo Framebuffer interface unit.

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


Framebuffer
===========
 

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Framebuffer;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,HeapManager,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Framebuffer specific constants}
 FRAMEBUFFER_NAME_PREFIX = 'Framebuffer';    {Name prefix for Framebuffer Devices}

 {Framebuffer Device Types}
 FRAMEBUFFER_TYPE_NONE        = 0;
 FRAMEBUFFER_TYPE_HARDWARE    = 1;
 FRAMEBUFFER_TYPE_VIRTUAL     = 2;
 
 {Framebuffer Device States}
 FRAMEBUFFER_STATE_DISABLED   = 0;
 FRAMEBUFFER_STATE_ENABLED    = 1;
 
 {Framebuffer Device Flags}
 FRAMEBUFFER_FLAG_NONE      = $00000000;
 FRAMEBUFFER_FLAG_DMA       = $00000001;  {If set the framebuffer supports DMA for read/write operations}
 FRAMEBUFFER_FLAG_MARK      = $00000002;  {If set the framebuffer requires mark after write operations}
 FRAMEBUFFER_FLAG_COMMIT    = $00000004;  {If set the framebuffer requires commit after write operations}
 FRAMEBUFFER_FLAG_BLANK     = $00000008;  {If set the framebuffer supports blanking the screen}
 FRAMEBUFFER_FLAG_CACHED    = $00000010;  {If set framebuffer is in cached memory and cache cleaning should be used}
 FRAMEBUFFER_FLAG_SWAP      = $00000020;  {If set framebuffer requires byte order of colors to be reversed (BGR <-> RGB)} 
 FRAMEBUFFER_FLAG_BACKLIGHT = $00000040;  {If set the framebuffer supports setting the backlight brightness}  
 FRAMEBUFFER_FLAG_VIRTUAL   = $00000080;  {If set the framebuffer supports virtual width and height}
 FRAMEBUFFER_FLAG_OFFSETX   = $00000100;  {If set the framebuffer supports virtual offset X (Horizontal Pan/Flip etc)}
 FRAMEBUFFER_FLAG_OFFSETY   = $00000200;  {If set the framebuffer supports virtual offset Y (Vertical Pan/Flip etc)}
 FRAMEBUFFER_FLAG_SYNC      = $00000400;  {If set the framebuffer supports waiting for vertical sync}
 
 {Framebuffer Transfer Flags}
 FRAMEBUFFER_TRANSFER_NONE = $00000000;
 FRAMEBUFFER_TRANSFER_DMA  = $00000001; {Use DMA for transfer operations (Note: Buffers must be DMA compatible)}
 
{==============================================================================}
type
 {Framebuffer specific types}
 PFramebufferPalette = ^TFramebufferPalette;
 TFramebufferPalette = record
  Start:LongWord;                    {The number of the first valid entry in the palette}
  Count:LongWord;                    {The total number of entries in the palette}
  Entries:array[0..255] of LongWord; {The palette entries in COLOR_FORMAT_DEFAULT format}
 end;
 
 PFramebufferProperties = ^TFramebufferProperties;
 TFramebufferProperties = record
  Flags:LongWord;                                {Framebuffer device flags (eg FRAMEBUFFER_FLAG_COMMIT) (Ignored for Allocate / SetProperties)}
  Address:LongWord;                              {Framebuffer address (Ignored for Allocate / SetProperties)}
  Size:LongWord;                                 {Framebuffer size (Bytes) (Ignored for Allocate / SetProperties)}
  Pitch:LongWord;                                {Framebuffer pitch (Bytes per Line) (Ignored for Allocate / SetProperties)}
  Depth:LongWord;                                {Framebuffer depth (Bits per Pixel)(8/16/24/32)}
  Order:LongWord;                                {Framebuffer pixel order (BGR/RGB)}
  Mode:LongWord;                                 {Framebuffer alpha mode (Enabled/Reversed/Ignored)}
  Format:LongWord;                               {Framebuffer color format (eg COLOR_FORMAT_ARGB32) (Ignored for Allocate / SetProperties)}
  PhysicalWidth:LongWord;                        {Framebuffer Physical Width (Pixels)}
  PhysicalHeight:LongWord;                       {Framebuffer Physical Height (Pixels)}
  VirtualWidth:LongWord;                         {Framebuffer Virtual Width (Pixels)}
  VirtualHeight:LongWord;                        {Framebuffer Virtual Height (Pixels)}
  OffsetX:LongWord;                              {Framebuffer Virtual Offset X (Pixels)}
  OffsetY:LongWord;                              {Framebuffer Virtual Offset Y (Pixels)}
  OverscanTop:LongWord;                          {Framebuffer Overscan Top (Pixels)}    
  OverscanBottom:LongWord;                       {Framebuffer Overscan Bottom (Pixels)}                                  
  OverscanLeft:LongWord;                         {Framebuffer Overscan Left (Pixels)}                                
  OverscanRight:LongWord;                        {Framebuffer Overscan Right (Pixels)}                                 
  Rotation:LongWord;                             {Framebuffer Rotation (eg FRAMEBUFFER_ROTATION_180)}
 end;
 
 PFramebufferDevice = ^TFramebufferDevice;
 
 {Framebuffer Enumeration Callback}
 TFramebufferEnumerate = function(Framebuffer:PFramebufferDevice;Data:Pointer):LongWord;
 {Framebuffer Notification Callback}
 TFramebufferNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {Framebuffer Device Methods}
 TFramebufferDeviceAllocate = function(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
 TFramebufferDeviceRelease = function(Framebuffer:PFramebufferDevice):LongWord;
 
 TFramebufferDeviceBlank = function(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
 
 TFramebufferDeviceRead = function(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Len,Flags:LongWord):LongWord;
 TFramebufferDeviceWrite = function(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Len,Flags:LongWord):LongWord;

 TFramebufferDeviceMark = function(Framebuffer:PFramebufferDevice;X,Y,Width,Height,Flags:LongWord):LongWord;
 TFramebufferDeviceCommit = function(Framebuffer:PFramebufferDevice;Address,Size,Flags:LongWord):LongWord;
 
 TFramebufferDeviceGetRect = function(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Skip,Flags:LongWord):LongWord;
 TFramebufferDevicePutRect = function(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Skip,Flags:LongWord):LongWord;
 TFramebufferDeviceCopyRect = function(Framebuffer:PFramebufferDevice;X1,Y1,X2,Y2,Width,Height,Flags:LongWord):LongWord;
 TFramebufferDeviceFillRect = function(Framebuffer:PFramebufferDevice;X,Y,Width,Height,Color,Flags:LongWord):LongWord;
 
 TFramebufferDeviceGetLine = function(Framebuffer:PFramebufferDevice;Y:LongWord):Pointer;
 TFramebufferDeviceGetPoint = function(Framebuffer:PFramebufferDevice;X,Y:LongWord):Pointer;
 
 TFramebufferDeviceWaitSync = function(Framebuffer:PFramebufferDevice):LongWord;
 
 TFramebufferDeviceGetOffset = function(Framebuffer:PFramebufferDevice;var X,Y:LongWord):LongWord;
 TFramebufferDeviceSetOffset = function(Framebuffer:PFramebufferDevice;X,Y:LongWord;Pan:Boolean):LongWord;
 
 TFramebufferDeviceGetPalette = function(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;
 TFramebufferDeviceSetPalette = function(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;
 
 TFramebufferDeviceSetBacklight = function(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
 
 TFramebufferDeviceGetProperties = function(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
 TFramebufferDeviceSetProperties = function(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
 
 {Framebuffer Device}
 TFramebufferDevice = record
  {Device Properties}
  Device:TDevice;                                {The Device entry for this Framebuffer device}
  {Framebuffer Properties}
  FramebufferId:LongWord;                        {Unique Id of this Framebuffer device in the Framebuffer device table}
  FramebufferState:LongWord;                     {Framebuffer device state (eg FRAMEBUFFER_STATE_ENABLED)}
  DeviceAllocate:TFramebufferDeviceAllocate;     {A device specific DeviceAllocate method implementing a standard framebuffer device interface (Mandatory)}
  DeviceRelease:TFramebufferDeviceRelease;       {A device specific DeviceRelease method implementing a standard framebuffer device interface (Mandatory)}
  DeviceBlank:TFramebufferDeviceBlank;           {A device specific DeviceBlank method implementing a standard framebuffer device interface (Optional)}
  DeviceRead:TFramebufferDeviceRead;             {A device specific DeviceRead method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
  DeviceWrite:TFramebufferDeviceWrite;           {A device specific DeviceWrite method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
  DeviceMark:TFramebufferDeviceMark;             {A device specific DeviceMark method implementing a standard framebuffer device interface (Optional)}
  DeviceCommit:TFramebufferDeviceCommit;         {A device specific DeviceCommit method implementing a standard framebuffer device interface (Optional)}
  DeviceGetRect:TFramebufferDeviceGetRect;       {A device specific DeviceGetRect method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
  DevicePutRect:TFramebufferDevicePutRect;       {A device specific DevicePutRect method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
  DeviceCopyRect:TFramebufferDeviceCopyRect;     {A device specific DeviceCopyRect method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
  DeviceFillRect:TFramebufferDeviceFillRect;     {A device specific DeviceFillRect method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
  DeviceGetLine:TFramebufferDeviceGetLine;       {A device specific DeviceGetLine method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
  DeviceGetPoint:TFramebufferDeviceGetPoint;     {A device specific DeviceGetPoint method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
  DeviceWaitSync:TFramebufferDeviceWaitSync;     {A device specific DeviceWaitSync method implementing a standard framebuffer device interface (Optional)}
  DeviceGetOffset:TFramebufferDeviceGetOffset;   {A device specific DeviceGetOffset method implementing a standard framebuffer device interface (Optional)}
  DeviceSetOffset:TFramebufferDeviceSetOffset;   {A device specific DeviceSetOffset method implementing a standard framebuffer device interface (Optional)}
  DeviceGetPalette:TFramebufferDeviceGetPalette; {A device specific DeviceGetPalette method implementing a standard framebuffer device interface (Optional)}
  DeviceSetPalette:TFramebufferDeviceSetPalette; {A device specific DeviceSetPalette method implementing a standard framebuffer device interface (Optional)}
  DeviceSetBacklight:TFramebufferDeviceSetBacklight;   {A device specific DeviceSetBacklight method implementing a standard framebuffer device interface (Optional)}
  DeviceGetProperties:TFramebufferDeviceGetProperties; {A device specific DeviceGetProperties method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
  DeviceSetProperties:TFramebufferDeviceSetProperties; {A device specific DeviceSetProperties method implementing a standard framebuffer device interface (Mandatory)}
  {Statistics Properties}
  AllocateCount:LongWord;
  ReleaseCount:LongWord;
  ReadCount:LongWord;
  WriteCount:LongWord;
  GetCount:LongWord;
  PutCount:LongWord;
  CopyCount:LongWord;
  FillCount:LongWord;
  {Driver Properties}
  Lock:TMutexHandle;                             {Device lock}
  Address:LongWord;                              {Framebuffer address}
  Size:LongWord;                                 {Framebuffer size (Bytes)}
  Pitch:LongWord;                                {Framebuffer pitch (Bytes per Line)}
  Depth:LongWord;                                {Framebuffer depth (Bits per Pixel)(8/16/24/32)}
  Order:LongWord;                                {Framebuffer pixel order (BGR/RGB)}
  Mode:LongWord;                                 {Framebuffer alpha mode (Enabled/Reversed/Ignored)}
  Format:LongWord;                               {Framebuffer color format (eg COLOR_FORMAT_ARGB32)}
  PhysicalWidth:LongWord;                        {Framebuffer Physical Width (Pixels)}
  PhysicalHeight:LongWord;                       {Framebuffer Physical Height (Pixels)}
  VirtualWidth:LongWord;                         {Framebuffer Virtual Width (Pixels)}
  VirtualHeight:LongWord;                        {Framebuffer Virtual Height (Pixels)}
  OffsetX:LongWord;                              {Framebuffer Virtual Offset X (Pixels)}
  OffsetY:LongWord;                              {Framebuffer Virtual Offset Y (Pixels)}
  OverscanTop:LongWord;                          {Framebuffer Overscan Top (Pixels)}    
  OverscanBottom:LongWord;                       {Framebuffer Overscan Bottom (Pixels)}                                  
  OverscanLeft:LongWord;                         {Framebuffer Overscan Left (Pixels)}                                
  OverscanRight:LongWord;                        {Framebuffer Overscan Right (Pixels)}                                 
  Rotation:LongWord;                             {Framebuffer Rotation (eg FRAMEBUFFER_ROTATION_180)}
  {Buffer Properties}
  LineBuffer:Pointer;                            {Buffer for line fills}
  CopyBuffer:Pointer;                            {Buffer for overlapped copy}
  {Internal Properties}
  Prev:PFramebufferDevice;                       {Previous entry in Framebuffer device table}
  Next:PFramebufferDevice;                       {Next entry in Framebuffer device table}
 end;

{==============================================================================}
{var}
 {Framebuffer specific variables}
 
{$IFDEF CONSOLE_EARLY_INIT}
var 
 {Init Handlers}
 FramebufferInitHandler:TFramebufferInit;
{$ENDIF}
 
{==============================================================================}
{Initialization Functions}
procedure FramebufferInit;

{==============================================================================}
{Framebuffer Functions}
function FramebufferDeviceAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function FramebufferDeviceRelease(Framebuffer:PFramebufferDevice):LongWord;

function FramebufferDeviceBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
 
function FramebufferDeviceRead(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Len,Flags:LongWord):LongWord;
function FramebufferDeviceWrite(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Len,Flags:LongWord):LongWord;

function FramebufferDeviceMark(Framebuffer:PFramebufferDevice;X,Y,Width,Height,Flags:LongWord):LongWord;
function FramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address,Size,Flags:LongWord):LongWord;
 
function FramebufferDeviceGetRect(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Skip,Flags:LongWord):LongWord;
function FramebufferDevicePutRect(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Skip,Flags:LongWord):LongWord;
function FramebufferDeviceCopyRect(Framebuffer:PFramebufferDevice;X1,Y1,X2,Y2,Width,Height,Flags:LongWord):LongWord;
function FramebufferDeviceFillRect(Framebuffer:PFramebufferDevice;X,Y,Width,Height,Color,Flags:LongWord):LongWord;

function FramebufferDeviceGetLine(Framebuffer:PFramebufferDevice;Y:LongWord):Pointer;
function FramebufferDeviceGetPoint(Framebuffer:PFramebufferDevice;X,Y:LongWord):Pointer;  

function FramebufferDeviceWaitSync(Framebuffer:PFramebufferDevice):LongWord;
 
function FramebufferDeviceGetOffset(Framebuffer:PFramebufferDevice;var X,Y:LongWord):LongWord;
function FramebufferDeviceSetOffset(Framebuffer:PFramebufferDevice;X,Y:LongWord;Pan:Boolean):LongWord;

function FramebufferDeviceGetPalette(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;
function FramebufferDeviceSetPalette(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;

function FramebufferDeviceSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;

function FramebufferDeviceGetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function FramebufferDeviceSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;

function FramebufferDeviceCheckFlag(Framebuffer:PFramebufferDevice;Flag:LongWord):Boolean;

function FramebufferDeviceGetFormat(Framebuffer:PFramebufferDevice):LongWord;

function FramebufferDeviceCreate:PFramebufferDevice;
function FramebufferDeviceCreateEx(Size:LongWord):PFramebufferDevice;
function FramebufferDeviceDestroy(Framebuffer:PFramebufferDevice):LongWord;

function FramebufferDeviceRegister(Framebuffer:PFramebufferDevice):LongWord;
function FramebufferDeviceDeregister(Framebuffer:PFramebufferDevice):LongWord;

function FramebufferDeviceFind(FramebufferId:LongWord):PFramebufferDevice;
function FramebufferDeviceFindByName(const Name:String):PFramebufferDevice; inline;
function FramebufferDeviceFindByDescription(const Description:String):PFramebufferDevice; inline;
function FramebufferDeviceEnumerate(Callback:TFramebufferEnumerate;Data:Pointer):LongWord;

function FramebufferDeviceNotification(Framebuffer:PFramebufferDevice;Callback:TFramebufferNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL Framebuffer Functions}
function SysFramebufferAvailable:Boolean;

{==============================================================================}
{Framebuffer Helper Functions}
function FramebufferDeviceGetCount:LongWord; inline;
function FramebufferDeviceGetDefault:PFramebufferDevice; inline;
function FramebufferDeviceSetDefault(Framebuffer:PFramebufferDevice):LongWord; 

function FramebufferDeviceCheck(Framebuffer:PFramebufferDevice):PFramebufferDevice;

function FramebufferDeviceSwap(Value:LongWord):LongWord; inline;

function FramebufferDepthToString(Depth:LongWord):String;
function FramebufferOrderToString(Order:LongWord):String;
function FramebufferModeToString(Mode:LongWord):String;
function FramebufferRotationToString(Rotation:LongWord):String;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Framebuffer specific variables}
 FramebufferInitialized:Boolean;
 
 FramebufferDeviceTable:PFramebufferDevice;
 FramebufferDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 FramebufferDeviceTableCount:LongWord;
 
 FramebufferDeviceDefault:PFramebufferDevice;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure FramebufferInit;
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if FramebufferInitialized then Exit;
 
 {$IFDEF CONSOLE_EARLY_INIT}
 {Initialize Device Support}
 DevicesInit;
 {$ENDIF}
 
 {Initialize Framebuffer Device Table}
 FramebufferDeviceTable:=nil;
 FramebufferDeviceTableLock:=CriticalSectionCreate; 
 FramebufferDeviceTableCount:=0;
 if FramebufferDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to create framebuffer device table lock');
  end;
 FramebufferDeviceDefault:=nil;

 {Register Platform Framebuffer Handlers}
 FramebufferAvailableHandler:=SysFramebufferAvailable;
 
 {Setup Framebuffer Width / Height}
 if (FRAMEBUFFER_DEFAULT_WIDTH = 0) or (FRAMEBUFFER_DEFAULT_HEIGHT = 0) then
  begin
   {Get Dimensions Width and Height}
   Status:=FramebufferGetDimensions(FRAMEBUFFER_DEFAULT_WIDTH,FRAMEBUFFER_DEFAULT_HEIGHT,FRAMEBUFFER_DEFAULT_OVERSCAN_TOP,FRAMEBUFFER_DEFAULT_OVERSCAN_BOTTOM,FRAMEBUFFER_DEFAULT_OVERSCAN_LEFT,FRAMEBUFFER_DEFAULT_OVERSCAN_RIGHT);
   if Status <> ERROR_SUCCESS then
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'FramebufferGetDimensions failed: ' + ErrorToString(Status));
     
     {$IFDEF CONSOLE_EARLY_INIT}
     {Set Dimension Defaults}
     FRAMEBUFFER_DEFAULT_WIDTH:=640;
     FRAMEBUFFER_DEFAULT_HEIGHT:=480;
     {$ENDIF}
    end;
  end;
 
 {Setup Framebuffer Depth}
 if FRAMEBUFFER_DEFAULT_DEPTH = 0 then
  begin
   FRAMEBUFFER_DEFAULT_DEPTH:=FRAMEBUFFER_DEPTH_32;
  end;  
  
 {$IFDEF CONSOLE_EARLY_INIT}
 {Check Init Handler}
 if Assigned(FramebufferInitHandler) then
  begin
   {Call Init Handler}
   FramebufferInitHandler;
  end;
 {$ENDIF}
 
 FramebufferInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Framebuffer Functions}
function FramebufferDeviceAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Allocate and enable a framebuffer device using supplied properties or defaults}
{Framebuffer: The framebuffer device to allocate}
{Properties: The framebuffer properties (Width/Height/Depth etc) to use for allocation (Optional)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 {if Properties = nil then Exit;} {Allow nil for defaults}
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Allocate');
 {$ENDIF}

 {Check Disabled}
 Result:=ERROR_SUCCESS;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_DISABLED then Exit;

 {Check Allocate}
 Result:=ERROR_INVALID_PARAMETER;
 if Assigned(Framebuffer.DeviceAllocate) then
  begin
   {Call Device Allocate}
   Result:=Framebuffer.DeviceAllocate(Framebuffer,Properties);
   if Result <> ERROR_SUCCESS then Exit;
  end; 
 
 {Enable Device}
 Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_ENABLED;

 {Notify Enable}
 NotifierNotify(@Framebuffer.Device,DEVICE_NOTIFICATION_ENABLE);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FramebufferDeviceRelease(Framebuffer:PFramebufferDevice):LongWord;
{Disable and release a framebuffer device}
{Framebuffer: The framebuffer device to release}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Release');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 {Check Release}
 if Assigned(Framebuffer.DeviceRelease) then
  begin
   {Call Device Release}
   Result:=Framebuffer.DeviceRelease(Framebuffer);
   if Result <> ERROR_SUCCESS then Exit;
  end; 
 
 {Disable Device}
 Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;

 {Notify Disable}
 NotifierNotify(@Framebuffer.Device,DEVICE_NOTIFICATION_DISABLE);
 
 {Update Framebuffer}
 {Buffers}
 if Framebuffer.LineBuffer <> nil then
  begin
   if DMAAvailable then DMAReleaseBuffer(Framebuffer.LineBuffer) else FreeMem(Framebuffer.LineBuffer);
   Framebuffer.LineBuffer:=nil;
  end;
 if Framebuffer.CopyBuffer <> nil then
  begin
   if DMAAvailable then DMAReleaseBuffer(Framebuffer.CopyBuffer) else FreeMem(Framebuffer.CopyBuffer);
   Framebuffer.CopyBuffer:=nil;
  end;
  
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FramebufferDeviceBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
{Blank (Turn off) the display of a framebuffer device}
{Framebuffer: The framebuffer device to blank}
{Blank: Turn off the display if True / Turn on the display if False}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Not all framebuffer devices support blank, returns ERROR_CALL_NOT_IMPLEMENTED if not supported}
{      Devices that support blank should set the flag FRAMEBUFFER_FLAG_BLANK}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Blank (Blank=' + BooleanToString(Blank) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceBlank) then
  begin
   Result:=Framebuffer.DeviceBlank(Framebuffer,Blank);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end; 
end;

{==============================================================================}
 
function FramebufferDeviceRead(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Len,Flags:LongWord):LongWord;
{Read one or more pixels from framebuffer device memory to a supplied buffer}
{Framebuffer: The framebuffer device to read from}
{X: The column to start reading from}
{Y: The row to start reading from}
{Buffer: Pointer to a buffer to receive the read pixels}
{Len: The number of pixels to read starting at X,Y}
{Flags: The flags for the transfer (eg FRAMEBUFFER_TRANSFER_DMA)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Pixel data will be returned in the color format of the framebuffer}
{Note: X and Y are relative to the physical screen and will be translated to the virtual buffer (Where applicable)}
{Note: The default method assumes that framebuffer memory is DMA coherent and does not require cache cleaning before a DMA read}
var
 Data:TDMAData;
 Size:LongWord;
 Address:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Len}
 if Len = 0 then Exit;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Read (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Len=' + IntToStr(Len) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceRead) then
  begin
   Result:=Framebuffer.DeviceRead(Framebuffer,X,Y,Buffer,Len,Flags);
  end
 else
  begin
   {Lock Framebuffer}
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   try
    {Check Address}
    if Framebuffer.Address = 0 then Exit;
    
    {Check X, Y}
    if X >= Framebuffer.PhysicalWidth then Exit;
    if Y >= Framebuffer.PhysicalHeight then Exit;
    
    {Get Size}
    Size:=Len * (Framebuffer.Depth shr 3);

    {Get Address}
    Address:=(Framebuffer.Address + ((Framebuffer.OffsetY + Y) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X) * (Framebuffer.Depth shr 3)));
    
    {Check Size}
    if (Address + Size) > (Framebuffer.Address + Framebuffer.Size) then Exit;
    
    {Check DMA Available}
    if not(DMAAvailable) or not(SysInitCompleted) then
     begin
      Flags:=Flags and not(FRAMEBUFFER_TRANSFER_DMA);
     end;
    
    {Check DMA Transfer}
    if ((Flags and FRAMEBUFFER_TRANSFER_DMA) <> 0) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) then
     begin
      {Check Cache}
      if not(DMA_CACHE_COHERENT) then
       begin
        {Clean Cache (Dest)}
        CleanDataCacheRange(LongWord(Buffer),Size);
       end;

      {Create Data}
      FillChar(Data,SizeOf(TDMAData),0);
      Data.Source:=Pointer(Address);
      Data.Dest:=Buffer;
      Data.Flags:=DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_BULK;
      Data.StrideLength:=0;
      Data.SourceStride:=0;
      Data.DestStride:=0;
      Data.Size:=Size;
      
      {Perform Read}
      DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
     end
    else
     begin
      {Read Pixels}
      System.Move(Pointer(Address)^,Buffer^,Size);
      
      {Memory Barrier}
      DataMemoryBarrier;  {After the Last Read}
     end;
    
    {Update Statistics}
    Inc(Framebuffer.ReadCount);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
   end; 
  end;  
end;

{==============================================================================}

function FramebufferDeviceWrite(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Len,Flags:LongWord):LongWord;
{Write one or more pixels to framebuffer device memory from a supplied buffer}
{Framebuffer: The framebuffer device to write to}
{X: The column to start writing from}
{Y: The row to start writing from}
{Buffer: Pointer to a buffer containing the pixels to write}
{Len: The number of pixels to write starting at X,Y}
{Flags: The flags for the transfer (eg FRAMEBUFFER_TRANSFER_DMA)} 
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must ensure pixel data is in the correct color format for the framebuffer}
{Note: X and Y are relative to the physical screen and will be translated to the virtual buffer (Where applicable)}
{Note: The default method assumes that framebuffer memory is DMA coherent and does not require cache invalidation after a DMA write}
var
 Data:TDMAData;
 Size:LongWord;
 Address:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Len}
 if Len = 0 then Exit;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Write (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Len=' + IntToStr(Len) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceWrite) then
  begin
   Result:=Framebuffer.DeviceWrite(Framebuffer,X,Y,Buffer,Len,Flags);
  end
 else
  begin
   {Lock Framebuffer}
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   try
    {Check Address}
    if Framebuffer.Address = 0 then Exit;
   
    {Check X, Y}
    if X >= Framebuffer.PhysicalWidth then Exit;
    if Y >= Framebuffer.PhysicalHeight then Exit;
    
    {Get Size}
    Size:=Len * (Framebuffer.Depth shr 3);

    {Get Address}
    Address:=(Framebuffer.Address + ((Framebuffer.OffsetY + Y) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X) * (Framebuffer.Depth shr 3)));

    {Check Size}
    if (Address + Size) > (Framebuffer.Address + Framebuffer.Size) then Exit;
    
    {Check DMA Available}
    if not(DMAAvailable) or not(SysInitCompleted) then
     begin
      Flags:=Flags and not(FRAMEBUFFER_TRANSFER_DMA);
     end;
    
    {Check DMA Transfer}
    if ((Flags and FRAMEBUFFER_TRANSFER_DMA) <> 0) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) then
     begin
      {Create Data}
      FillChar(Data,SizeOf(TDMAData),0);
      Data.Source:=Buffer;
      Data.Dest:=Pointer(Address);
      Data.Flags:=DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
      Data.StrideLength:=0;
      Data.SourceStride:=0;
      Data.DestStride:=0;
      Data.Size:=Size;
      
      {Perform Write}
      DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
     end
    else
     begin
      {Memory Barrier}
      DataMemoryBarrier;  {Before the First Write}
     
      {Write Pixels}
      System.Move(Buffer^,Pointer(Address)^,Size);
     end;
   
    {Check Commit}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_COMMIT) <> 0) and Assigned(Framebuffer.DeviceCommit) then
     begin
      Framebuffer.DeviceCommit(Framebuffer,Address,Size,Flags);
     end;
    
    {Check Mark}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_MARK) <> 0) and Assigned(Framebuffer.DeviceMark) then
     begin
      {Assume full lines}
      Framebuffer.DeviceMark(Framebuffer,0,Y,Framebuffer.VirtualWidth,(Size div Framebuffer.Pitch) + 1,Flags);
     end;
   
    {Update Statistics}
    Inc(Framebuffer.WriteCount);
   
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
   end; 
  end;  
end;

{==============================================================================}

function FramebufferDeviceMark(Framebuffer:PFramebufferDevice;X,Y,Width,Height,Flags:LongWord):LongWord;
{Mark a region written to the framebuffer and signal the device to take any neccessary actions}
{Framebuffer: The framebuffer device to mark}
{X: The starting column of the mark}
{Y: The starting row of the mark}
{Width: The number of columns to mark}
{Height: The number of rows to mark}
{Flags: The flags used for the transfer (eg FRAMEBUFFER_TRANSFER_DMA)} 
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: X and Y are relative to the physical screen and will be translated to the virtual buffer (Where applicable)}
{Note: Not all framebuffer devices support mark, returns ERROR_CALL_NOT_IMPLEMENTED if not supported}
{      Devices that support and require mark should set the flag FRAMEBUFFER_FLAG_MARK}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Mark (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceMark) then
  begin
   Result:=Framebuffer.DeviceMark(Framebuffer,X,Y,Width,Height,Flags);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end; 
end;

{==============================================================================}

function FramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address,Size,Flags:LongWord):LongWord;
{Commit a region written to the framebuffer and signal the device to take any neccessary actions}
{Framebuffer: The framebuffer device to commit}
{Address: The starting address of the commit}
{Size: The size in bytes of the commit}
{Flags: The flags used for the transfer (eg FRAMEBUFFER_TRANSFER_DMA)} 
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Not all framebuffer devices support commit, returns ERROR_CALL_NOT_IMPLEMENTED if not supported}
{      Devices that support and require commit should set the flag FRAMEBUFFER_FLAG_COMMIT}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Commit (Address=' + IntToHex(Address,8) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceCommit) then
  begin
   Result:=Framebuffer.DeviceCommit(Framebuffer,Address,Size,Flags);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end; 
end;

{==============================================================================}
 
function FramebufferDeviceGetRect(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Skip,Flags:LongWord):LongWord;
{Get a rectangular area of pixels from framebuffer memory to a supplied buffer}
{Framebuffer: The framebuffer device to get from}
{X: The starting column of the get}
{Y: The starting row of the get}
{Buffer: Pointer to a block of memory large enough to hold the pixels in a contiguous block of rows}
{Width: The number of columns to get}
{Height: The number of rows to get}
{Skip: The number of pixels to skip in the buffer after each row (Optional)}
{Flags: The flags for the transfer (eg FRAMEBUFFER_TRANSFER_DMA)} 
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Pixel data will be returned in the color format of the framebuffer}
{Note: X and Y are relative to the physical screen and will be translated to the virtual buffer (Where applicable)}
{Note: The default method assumes that framebuffer memory is DMA coherent and does not require cache cleaning before a DMA read}
var
 Data:TDMAData;
 Size:LongWord;
 Count:LongWord;
 Stride:LongWord;
 Address:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Get Rect (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceGetRect) then
  begin
   Result:=Framebuffer.DeviceGetRect(Framebuffer,X,Y,Buffer,Width,Height,Skip,Flags);
  end
 else
  begin
   {Lock Framebuffer}
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   try
    {Check Address}
    if Framebuffer.Address = 0 then Exit;

    {Check X, Y}
    if X >= Framebuffer.PhysicalWidth then Exit;
    if Y >= Framebuffer.PhysicalHeight then Exit;
    if X + (Width - 1) >= Framebuffer.PhysicalWidth then Exit;
    if Y + (Height - 1) >= Framebuffer.PhysicalHeight then Exit;
    
    {Get Skip (Normally 0)}
    Skip:=Skip * (Framebuffer.Depth shr 3); 
    
    {Get Size}
    Size:=Width * (Framebuffer.Depth shr 3);
    
    {Get Stride}
    Stride:=Framebuffer.Pitch - Size;
    
    {Get Address}
    Address:=(Framebuffer.Address + ((Framebuffer.OffsetY + Y) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X) * (Framebuffer.Depth shr 3)));
    
    {Check DMA Available}
    if not(DMAAvailable) or not(SysInitCompleted) then
     begin
      Flags:=Flags and not(FRAMEBUFFER_TRANSFER_DMA);
     end;
    
    {Check DMA Transfer}
    if ((Flags and FRAMEBUFFER_TRANSFER_DMA) <> 0) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) then
     begin
      {Check Cache}
      if not(DMA_CACHE_COHERENT) then
       begin
        {Clean Cache (Dest)}
        CleanDataCacheRange(LongWord(Buffer),(Size + Skip) * Height);
       end;

      {Create Data}
      FillChar(Data,SizeOf(TDMAData),0);
      Data.Source:=Pointer(Address);
      Data.Dest:=Buffer;
      Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_BULK;
      Data.StrideLength:=Size;
      Data.SourceStride:=Stride;
      Data.DestStride:=Skip;
      Data.Size:=Data.StrideLength * Height;
      
      {Perform Get}
      DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
     end
    else
     begin
      {Get Rectangle}
      Count:=Y;
      while Count <= Y + (Height - 1) do
       begin
        {Read Pixels}
        System.Move(Pointer(Address)^,Buffer^,Size);
        
        {Next Line}
        Inc(Count);
        Inc(Address,Size + Stride);
        Inc(Buffer,Size + Skip);
       end; 
       
      {Memory Barrier}
      DataMemoryBarrier;  {After the Last Read}
     end; 
   
    {Update Statistics}
    Inc(Framebuffer.GetCount);
   
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
   end; 
  end;  
end;

{==============================================================================}

function FramebufferDevicePutRect(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Skip,Flags:LongWord):LongWord;
{Put a rectangular area of pixels from a supplied buffer to framebuffer memory}
{Framebuffer: The framebuffer device to put to}
{X: The starting column of the put}
{Y: The starting row of the put}
{Buffer: Pointer to a block of memory containing the pixels in a contiguous block of rows}
{Width: The number of columns to put}
{Height: The number of rows to put}
{Skip: The number of pixels to skip in the buffer after each row (Optional)}
{Flags: The flags for the transfer (eg FRAMEBUFFER_TRANSFER_DMA)} 
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must ensure pixel data is in the correct color format for the framebuffer}
{Note: X and Y are relative to the physical screen and will be translated to the virtual buffer (Where applicable)}
{Note: The default method assumes that framebuffer memory is DMA coherent and does not require cache invalidation after a DMA write}
var
 Data:TDMAData;
 Size:LongWord;
 Count:LongWord;
 Start:PtrUInt;
 Stride:LongWord;
 Address:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Put Rect (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DevicePutRect) then
  begin
   Result:=Framebuffer.DevicePutRect(Framebuffer,X,Y,Buffer,Width,Height,Skip,Flags);
  end
 else
  begin
   {Lock Framebuffer}
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   try
    {Check Address}
    if Framebuffer.Address = 0 then Exit;

    {Check X, Y}
    if X >= Framebuffer.PhysicalWidth then Exit;
    if Y >= Framebuffer.PhysicalHeight then Exit;
    if X + (Width - 1) >= Framebuffer.PhysicalWidth then Exit;
    if Y + (Height - 1) >= Framebuffer.PhysicalHeight then Exit;
    
    {Get Skip (Normally 0)}
    Skip:=Skip * (Framebuffer.Depth shr 3); 
    
    {Get Size}
    Size:=Width * (Framebuffer.Depth shr 3);
    
    {Get Stride}
    Stride:=Framebuffer.Pitch - Size;
    
    {Get Address}
    Address:=(Framebuffer.Address + ((Framebuffer.OffsetY + Y) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X) * (Framebuffer.Depth shr 3)));

    {Check DMA Available}
    if not(DMAAvailable) or not(SysInitCompleted) then
     begin
      Flags:=Flags and not(FRAMEBUFFER_TRANSFER_DMA);
     end;
    
    {Check DMA Transfer}
    if ((Flags and FRAMEBUFFER_TRANSFER_DMA) <> 0) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) then
     begin
      {Create Data}
      FillChar(Data,SizeOf(TDMAData),0);
      Data.Source:=Buffer;
      Data.Dest:=Pointer(Address);
      Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
      Data.StrideLength:=Size;
      Data.SourceStride:=Skip;
      Data.DestStride:=Stride;
      Data.Size:=Data.StrideLength * Height;
      
      {Perform Put}
      DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
     end
    else
     begin
      {Memory Barrier}
      DataMemoryBarrier;  {Before the First Write}

      {Get Start (Save Address)}
      Start:=Address;
      
      {Put Rectangle}
      Count:=Y;
      while Count <= Y + (Height - 1) do
       begin
        {Write Pixels}
        System.Move(Buffer^,Pointer(Start)^,Size);
        
        {Next Line}
        Inc(Count);
        Inc(Buffer,Size + Skip);
        Inc(Start,Size + Stride);
       end; 
     end; 
   
    {Check Commit}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_COMMIT) <> 0) and Assigned(Framebuffer.DeviceCommit) then
     begin
      Framebuffer.DeviceCommit(Framebuffer,Address,(Size + Stride) * Height,Flags);
     end;
    
    {Check Mark}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_MARK) <> 0) and Assigned(Framebuffer.DeviceMark) then
     begin
      Framebuffer.DeviceMark(Framebuffer,X,Y,Width,Height,Flags);
     end;
   
    {Update Statistics}
    Inc(Framebuffer.PutCount);
   
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
   end; 
  end;  
end;

{==============================================================================}

function FramebufferDeviceCopyRect(Framebuffer:PFramebufferDevice;X1,Y1,X2,Y2,Width,Height,Flags:LongWord):LongWord;
{Copy a rectangular area of pixels within framebuffer memory}
{Framebuffer: The framebuffer device to copy on}
{X1: The starting column to copy from}
{Y1: The starting row to copy from}
{X2: The starting column to copy to}
{Y2: The starting row to copy to}
{Width: The number of columns to copy}
{Height: The number of rows to copy}
{Flags: The flags for the transfer (eg FRAMEBUFFER_TRANSFER_DMA)} 
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: X1, Y1, X2 and Y2 are relative to the physical screen and will be translated to the virtual buffer (Where applicable)}
{Note: The default method assumes that framebuffer memory is DMA coherent and does not require cache clean/invalidate before or after a DMA read/write}
var
 Data:TDMAData;
 Next:PDMAData;
 First:PDMAData;
 Size:LongWord;
 Lines:LongWord;
 Count:LongWord;
 Start:PtrUInt;
 Buffer:Pointer;
 Stride:LongInt; {Allow for negative stride}
 Source:PtrUInt;
 Address:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Copy Rect (X1=' + IntToStr(X1) + ' Y1=' + IntToStr(Y1) + ' X2=' + IntToStr(X2) + ' Y2=' + IntToStr(Y2) + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceCopyRect) then
  begin
   Result:=Framebuffer.DeviceCopyRect(Framebuffer,X1,Y1,X2,Y2,Width,Height,Flags);
  end
 else
  begin
   {Lock Framebuffer}
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   try
    {Check Address}
    if Framebuffer.Address = 0 then Exit;

    {Check X1, Y1}
    if X1 >= Framebuffer.PhysicalWidth then Exit;
    if Y1 >= Framebuffer.PhysicalHeight then Exit;
    if X1 + (Width - 1) >= Framebuffer.PhysicalWidth then Exit;
    if Y1 + (Height - 1) >= Framebuffer.PhysicalHeight then Exit;

    {Check X2, Y2}
    if X2 >= Framebuffer.PhysicalWidth then Exit;
    if Y2 >= Framebuffer.PhysicalHeight then Exit;
    if X2 + (Width - 1) >= Framebuffer.PhysicalWidth then Exit;
    if Y2 + (Height - 1) >= Framebuffer.PhysicalHeight then Exit;
    
    {Get Size}
    Size:=Width * (Framebuffer.Depth shr 3);
    
    {Check DMA Available}
    if not(DMAAvailable) or not(SysInitCompleted) then
     begin
      Flags:=Flags and not(FRAMEBUFFER_TRANSFER_DMA);
     end;
    
    {Check Direction}
    if (Y1 = Y2) and (X2 > X1) then
     begin
      {Overlapped Copy}
      {Get Stride}
      Stride:=Framebuffer.Pitch - Size;
      
      {Check Buffer}
      if Framebuffer.CopyBuffer = nil then
       begin
        {Get Buffer}
        if DMAAvailable then
         begin
          {Allocate DMA Buffer}
          Framebuffer.CopyBuffer:=DMAAllocateBuffer(SIZE_64K);
         end
        else
         begin
          {Allocate Normal Buffer (No DMA)}
          if SysInitCompleted then Framebuffer.CopyBuffer:=GetMem(SIZE_64K);
         end;
       end;
      {Get Buffer}
      Buffer:=Framebuffer.CopyBuffer;
      if Buffer = nil then Buffer:=GetMem(SIZE_64K);
      if Buffer = nil then Exit;
      
      {Get Source}
      Source:=(Framebuffer.Address + ((Framebuffer.OffsetY + Y1) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X1) * (Framebuffer.Depth shr 3)));
      
      {Get Address}
      Address:=(Framebuffer.Address + ((Framebuffer.OffsetY + Y2) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X2) * (Framebuffer.Depth shr 3)));
      
      {Check DMA Transfer}
      if ((Flags and FRAMEBUFFER_TRANSFER_DMA) <> 0) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) then
       begin
        {Check Cached}
        if not(DMA_CACHE_COHERENT) then
         begin
          {Clean Cache}
          CleanDataCacheRange(LongWord(Buffer),SIZE_64K);
         end;
         
        {Get Lines}
        Lines:=(SIZE_64K - SIZE_16K) div Size;
        
        {Get First}
        First:=PDMAData(Buffer + (Size * Lines));
        Next:=First;
        
        {Create Data}
        Count:=Y1;
        while Count <= Y1 + (Height - 1) do
         begin
          {Check Lines}
          if ((Y1 + Height) - Count) < Lines then Lines:=((Y1 + Height) - Count);
          
          {Create Data (To Buffer)}
          FillChar(Next^,SizeOf(TDMAData),0);
          Next.Source:=Pointer(Framebuffer.Address + ((Framebuffer.OffsetY + Count) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X1) * (Framebuffer.Depth shr 3)));
          Next.Dest:=Buffer;
          Next.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
          Next.StrideLength:=Size;
          Next.SourceStride:=Stride;
          Next.DestStride:=Stride;
          Next.Size:=Size * Lines;
          Next.Next:=PDMAData(LongWord(Next) + SizeOf(TDMAData));
         
          {Get Next}
          Next:=Next.Next;
          
          {Create Data (From Buffer)}
          FillChar(Next^,SizeOf(TDMAData),0);
          Next.Source:=Buffer;
          Next.Dest:=Pointer(Framebuffer.Address + ((Framebuffer.OffsetY + Count) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X2) * (Framebuffer.Depth shr 3)));
          Next.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
          Next.StrideLength:=Size;
          Next.SourceStride:=Stride;
          Next.DestStride:=Stride;
          Next.Size:=Size * Lines;
          Next.Next:=PDMAData(LongWord(Next) + SizeOf(TDMAData));
          
          Inc(Count,Lines);
         
          if Count > Y1 + (Height - 1) then
           begin
            Next.Next:=nil;
           end
          else
           begin
            Next:=Next.Next;
           end;
         end; 
         
        {Perform Copy}
        DMATransfer(First,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
       end
      else
       begin
        {Memory Barrier}
        DataMemoryBarrier;  {Before the First Write}
      
        {Get Start (Save Address)}
        Start:=Address;
      
        {Copy Rectangle}
        Count:=Y1;
        while Count <= Y1 + (Height - 1) do
         begin
          {Copy Pixels (To Buffer)}
          System.Move(Pointer(Source)^,Buffer^,Size);
          
          {Copy Pixels (From Buffer)}
          System.Move(Buffer^,Pointer(Start)^,Size);
          
          {Next Line}
          Inc(Count);
          Inc(Source,Size + Stride);
          Inc(Start,Size + Stride);
         end; 
       
        {Memory Barrier}
        DataMemoryBarrier;  {After the Last Read}
       end; 
       
      {Check Buffer}
      if Buffer <> Framebuffer.CopyBuffer then FreeMem(Buffer);
     end
    else
     begin
      {Normal Copy}
      if Y2 <= Y1 then
       begin
        {Top to Bottom}
        {Get Stride}
        Stride:=Framebuffer.Pitch - Size;
        
        {Get Source}
        Source:=(Framebuffer.Address + ((Framebuffer.OffsetY + Y1) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X1) * (Framebuffer.Depth shr 3)));
        
        {Get Address}
        Address:=(Framebuffer.Address + ((Framebuffer.OffsetY + Y2) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X2) * (Framebuffer.Depth shr 3)));
        
        {Check DMA Transfer}
        if ((Flags and FRAMEBUFFER_TRANSFER_DMA) <> 0) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) then
         begin
          {Create Data}
          FillChar(Data,SizeOf(TDMAData),0);
          Data.Source:=Pointer(Source);
          Data.Dest:=Pointer(Address);
          Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
          Data.StrideLength:=Size;
          Data.SourceStride:=Stride;
          Data.DestStride:=Stride;
          Data.Size:=Data.StrideLength * Height;
          
          {Perform Copy}
          DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
         end
        else
         begin
          {Memory Barrier}
          DataMemoryBarrier;  {Before the First Write}
        
          {Get Start (Save Address)}
          Start:=Address;
        
          {Copy Rectangle}
          Count:=Y1;
          while Count <= Y1 + (Height - 1) do
           begin
            {Copy Pixels}
            System.Move(Pointer(Source)^,Pointer(Start)^,Size);
            
            {Next Line}
            Inc(Count);
            Inc(Source,Size + Stride);
            Inc(Start,Size + Stride);
           end; 
         
          {Memory Barrier}
          DataMemoryBarrier;  {After the Last Read}
         end; 
       end
      else
       begin
        {Bottom to Top}
        {Get Stride}
        Stride:=-(Framebuffer.Pitch + Size); {Negative Stride}
        
        {Get Source}
        Source:=(Framebuffer.Address + ((Framebuffer.OffsetY + Y1 + Height - 1) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X1) * (Framebuffer.Depth shr 3)));
        
        {Get Address}
        Address:=(Framebuffer.Address + ((Framebuffer.OffsetY + Y2 + Height - 1) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X2) * (Framebuffer.Depth shr 3)));
        
        {Check DMA Transfer}
        if ((Flags and FRAMEBUFFER_TRANSFER_DMA) <> 0) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) then
         begin
          {Create Data}
          FillChar(Data,SizeOf(TDMAData),0);
          Data.Source:=Pointer(Source);
          Data.Dest:=Pointer(Address);
          Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
          Data.StrideLength:=Size;
          Data.SourceStride:=Stride;
          Data.DestStride:=Stride;
          Data.Size:=Data.StrideLength * Height;
          
          {Perform Copy}
          DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
         end
        else
         begin
          {Memory Barrier}
          DataMemoryBarrier;  {Before the First Write}
          
          {Copy Rectangle}
          Count:=Y1 + (Height - 1);
          while Count >= Y1 do
           begin
            {Copy Pixels}
            System.Move(Pointer(Source)^,Pointer(Address)^,Size);

            {Next Line}
            Dec(Count);
            Inc(Source,Size + Stride); {Negative Stride}
            Inc(Address,Size + Stride); {Negative Stride}
           end; 
         
          {Memory Barrier}
          DataMemoryBarrier;  {After the Last Read}
         end; 
         
        {Get Stride (Positive - Required for commit)}
        Stride:=Framebuffer.Pitch - Size;
         
        {Get Address (Start - Required for commit)}
        Address:=(Framebuffer.Address + ((Framebuffer.OffsetY + Y2) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X2) * (Framebuffer.Depth shr 3)));
       end;       
     end;     
   
    {Check Commit}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_COMMIT) <> 0) and Assigned(Framebuffer.DeviceCommit) then
     begin
      Framebuffer.DeviceCommit(Framebuffer,Address,(Size + Stride) * Height,Flags);
     end;
    
    {Check Mark}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_MARK) <> 0) and Assigned(Framebuffer.DeviceMark) then
     begin
      Framebuffer.DeviceMark(Framebuffer,X2,Y2,Width,Height,Flags);
     end;
   
    {Update Statistics}
    Inc(Framebuffer.CopyCount);
   
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
   end; 
  end;  
end;

{==============================================================================}

function FramebufferDeviceFillRect(Framebuffer:PFramebufferDevice;X,Y,Width,Height,Color,Flags:LongWord):LongWord;
{Fill a rectangular area of pixels within framebuffer memory}
{Framebuffer: The framebuffer device to fill on}
{X: The starting column of the fill}
{Y: The starting row of the fill}
{Width: The number of columns to fill}
{Height: The number of rows to fill}
{Color: The color to use for the fill}
{Flags: The flags for the transfer (eg FRAMEBUFFER_TRANSFER_DMA)} 
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Color must be specified in the correct format for the framebuffer}
{Note: X and Y are relative to the physical screen and will be translated to the virtual buffer (Where applicable)}
{Note: The default method assumes that framebuffer memory is DMA coherent and does not require cache invalidation after a DMA write}
var
 Data:TDMAData;
 Size:LongWord;
 Count:LongWord;
 Start:PtrUInt;
 Buffer:Pointer;
 Stride:LongWord;
 Address:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;
 
 {Check Color}
 if Color = COLOR_NONE then Exit;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Fill Rect (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ' Color=' + IntToHex(Color,8) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceFillRect) then
  begin
   Result:=Framebuffer.DeviceFillRect(Framebuffer,X,Y,Width,Height,Color,Flags);
  end
 else
  begin
   {Lock Framebuffer}
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   try
    {Check Address}
    if Framebuffer.Address = 0 then Exit;

    {Check X, Y}
    if X >= Framebuffer.PhysicalWidth then Exit;
    if Y >= Framebuffer.PhysicalHeight then Exit;
    if X + (Width - 1) >= Framebuffer.PhysicalWidth then Exit;
    if Y + (Height - 1) >= Framebuffer.PhysicalHeight then Exit;
   
    {Get Size}
    Size:=Width * (Framebuffer.Depth shr 3);
    
    {Get Stride}
    Stride:=Framebuffer.Pitch - Size;
    
    {Get Address}
    Address:=(Framebuffer.Address + ((Framebuffer.OffsetY + Y) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X) * (Framebuffer.Depth shr 3)));

    {Check Buffer}
    if Framebuffer.LineBuffer = nil then
     begin
      {Get Buffer}
      if DMAAvailable then
       begin
        {Allocate DMA Buffer}
        Framebuffer.LineBuffer:=DMAAllocateBuffer(Framebuffer.Pitch);
       end
      else
       begin
        {Allocate Normal Buffer (No DMA)}
        if SysInitCompleted then Framebuffer.LineBuffer:=GetMem(Framebuffer.Pitch);
       end;
     end;
    {Get Buffer}
    Buffer:=Framebuffer.LineBuffer;
    if Buffer = nil then Buffer:=GetMem(Framebuffer.Pitch);
    if Buffer = nil then Exit;
    
    {Fill Source}
    Count:=0;
    case Framebuffer.Depth of
     FRAMEBUFFER_DEPTH_8:begin
       while Count < Size do
        begin
         PByte(Buffer + Count)^:=Color;
         
         Inc(Count,1);
        end;
      end;
     FRAMEBUFFER_DEPTH_16:begin
       while Count < Size do
        begin
         PWord(Buffer + Count)^:=Color;
         
         Inc(Count,2);
        end;
      end;
     FRAMEBUFFER_DEPTH_24:begin
       while Count < Size do
        begin
         PWord(Buffer + Count)^:=Color;
         PByte(Buffer + Count + 2)^:=Color shr 16;
         
         Inc(Count,3);
        end;
      end;
     FRAMEBUFFER_DEPTH_32:begin
       while Count < Size do
        begin
         PLongWord(Buffer + Count)^:=Color;
         
         Inc(Count,4);
        end;
      end;
    end;  
    
    {Check DMA Available}
    if not(DMAAvailable) or not(SysInitCompleted) then
     begin
      Flags:=Flags and not(FRAMEBUFFER_TRANSFER_DMA);
     end;
    
    {Check DMA Transfer}
    if ((Flags and FRAMEBUFFER_TRANSFER_DMA) <> 0) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) then
     begin
      {Check Cached}
      if not(DMA_CACHE_COHERENT) then
       begin
        {Clean Cache}
        CleanDataCacheRange(LongWord(Buffer),Size);
       end;
      
      {Create Data}
      FillChar(Data,SizeOf(TDMAData),0);
      Data.Source:=Buffer;
      Data.Dest:=Pointer(Address);
      Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
      Data.StrideLength:=Size;
      Data.SourceStride:=-Size; {Negative Stride}
      Data.DestStride:=Stride;
      Data.Size:=Data.StrideLength * Height;
       
      {Perform Fill}
      DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
     end
    else
     begin
      {Memory Barrier}
      DataMemoryBarrier;  {Before the First Write}
    
      {Get Start (Save Address)}
      Start:=Address;
    
      {Fill Rectangle}
      Count:=Y;
      while Count <= Y + (Height - 1) do
       begin
        {Write Pixels}
        System.Move(Buffer^,Pointer(Start)^,Size);
        
        {Next Line}
        Inc(Count);
        Inc(Start,Size + Stride);
       end; 
     end; 
   
    {Check Commit}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_COMMIT) <> 0) and Assigned(Framebuffer.DeviceCommit) then
     begin
      Framebuffer.DeviceCommit(Framebuffer,Address,(Size + Stride) * Height,Flags);
     end;
    
    {Check Mark}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_MARK) <> 0) and Assigned(Framebuffer.DeviceMark) then
     begin
      Framebuffer.DeviceMark(Framebuffer,X,Y,Width,Height,Flags);
     end;
   
    {Update Statistics}
    Inc(Framebuffer.FillCount);
   
    {Check Buffer}
    if Buffer <> Framebuffer.LineBuffer then FreeMem(Buffer);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
   end; 
  end;  
end;

{==============================================================================}

function FramebufferDeviceGetLine(Framebuffer:PFramebufferDevice;Y:LongWord):Pointer;
{Get the address of the start of a row in framebuffer memory}
{Framebuffer: The framebuffer device to get the start address from}
{Y: The row to get the start address of}
{Return: Pointer to the start address of the row or nil on failure}

{Note: Y is relative to the physical screen and will be translated to the virtual buffer (Where applicable)}
begin
 {}
 Result:=nil;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Get Line (Y=' + IntToStr(Y) + ')');
 {$ENDIF}

 {Check Enabled}
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceGetLine) then
  begin
   Result:=Framebuffer.DeviceGetLine(Framebuffer,Y);
  end
 else
  begin
   {Lock Framebuffer}
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   try
    {Check Address}
    if Framebuffer.Address = 0 then Exit;

    {Check Y}
    if Y >= Framebuffer.PhysicalHeight then Exit;
 
    {Get Address}
    Result:=Pointer(Framebuffer.Address + ((Framebuffer.OffsetY + Y) * Framebuffer.Pitch));
   finally
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
   end; 
  end;  
end;

{==============================================================================}

function FramebufferDeviceGetPoint(Framebuffer:PFramebufferDevice;X,Y:LongWord):Pointer;  
{Get the address of the specified row and column in framebuffer memory}
{Framebuffer: The framebuffer device to get the address from}
{X: The column to get the start address of}
{Y: The row to get the start address of}
{Return: Pointer to the address of the row and column or nil on failure}

{Note: X and Y are relative to the physical screen and will be translated to the virtual buffer (Where applicable)}
begin
 {}
 Result:=nil;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Get Point (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ')');
 {$ENDIF}

 {Check Enabled}
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceGetPoint) then
  begin
   Result:=Framebuffer.DeviceGetPoint(Framebuffer,X,Y);
  end
 else
  begin
   {Lock Framebuffer}
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   try
    {Check Address}
    if Framebuffer.Address = 0 then Exit;

    {Check X, Y}
    if X >= Framebuffer.PhysicalWidth then Exit;
    if Y >= Framebuffer.PhysicalHeight then Exit;
 
    {Get Address}
    Result:=Pointer(Framebuffer.Address + ((Framebuffer.OffsetY + Y) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X) * (Framebuffer.Depth shr 3)));
   finally
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
   end; 
  end;  
end;

{==============================================================================}

function FramebufferDeviceWaitSync(Framebuffer:PFramebufferDevice):LongWord;
{Wait for the next vertical sync signal from the display hardware}
{Framebuffer: The framebuffer device to wait for}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Not all framebuffer devices support wait sync, returns ERROR_CALL_NOT_IMPLEMENTED if not supported}
{      Devices that support wait sync should set the flag FRAMEBUFFER_FLAG_SYNC}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Wait Sync');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceWaitSync) then
  begin
   Result:=Framebuffer.DeviceWaitSync(Framebuffer);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end; 
end;

{==============================================================================}

function FramebufferDeviceGetOffset(Framebuffer:PFramebufferDevice;var X,Y:LongWord):LongWord;
{Get the virtual offset X and Y from a framebuffer device}
{Framebuffer: The framebuffer device to get the offset from}
{X: The X (Column) offset value in pixels returned from the device if successful}
{X: The Y (Row) offset value in pixels returned from the device if successful}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: X and Y are relative to the virtual buffer and NOT the physical screen (Where applicable)}
{Note: Not all framebuffer devices support X and/or Y offset}
{      Devices that support offset X should set the flag FRAMEBUFFER_FLAG_OFFSETX}
{      Devices that support offset Y should set the flag FRAMEBUFFER_FLAG_OFFSETY}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Get Offset (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ')');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceGetOffset) then
  begin
   Result:=Framebuffer.DeviceGetOffset(Framebuffer,X,Y);
  end
 else
  begin
   {Lock Framebuffer}
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   
   {Get X and Y}
   X:=Framebuffer.OffsetX;
   Y:=Framebuffer.OffsetY;
   
   {Return Result}
   Result:=ERROR_SUCCESS;
   
   {Unlock Framebuffer}
   MutexUnlock(Framebuffer.Lock);
  end; 
end;

{==============================================================================}

function FramebufferDeviceSetOffset(Framebuffer:PFramebufferDevice;X,Y:LongWord;Pan:Boolean):LongWord;
{Set the virtual offset X and Y of a framebuffer device}
{Framebuffer: The framebuffer device to set the offset for}
{X: The X (Column) offset value in pixels to set}
{X: The Y (Row) offset value in pixels to set}
{Pan: If True then pan the display without updating the Offset X and/or Y}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: X and Y are relative to the virtual buffer and NOT the physical screen (Where applicable)}
{Note: Not all framebuffer devices support X and/or Y offset, returns ERROR_CALL_NOT_IMPLEMENTED if not supported}
{      Devices that support offset X should set the flag FRAMEBUFFER_FLAG_OFFSETX}
{      Devices that support offset Y should set the flag FRAMEBUFFER_FLAG_OFFSETY}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Set Offset (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceSetOffset) then
  begin
   Result:=Framebuffer.DeviceSetOffset(Framebuffer,X,Y,Pan);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end; 
end;

{==============================================================================}

function FramebufferDeviceGetPalette(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;
{Get the 8 bit color palette from a framebuffer device}
{Framebuffer: The framebuffer device to get the palette from}
{Palette: Pointer to a TFramebufferPalette structure for the palette data}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Not all framebuffer devices support 8 bit palette, returns ERROR_CALL_NOT_IMPLEMENTED if not supported}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Get Palette');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceGetPalette) then
  begin
   Result:=Framebuffer.DeviceGetPalette(Framebuffer,Palette);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end; 
end;

{==============================================================================}

function FramebufferDeviceSetPalette(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;
{Set the 8 bit color palette of a framebuffer device}
{Framebuffer: The framebuffer device to set the palette for}
{Palette: Pointer to a TFramebufferPalette structure for the palette data}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Not all framebuffer devices support 8 bit palette, returns ERROR_CALL_NOT_IMPLEMENTED if not supported}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Set Palette');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceSetPalette) then
  begin
   Result:=Framebuffer.DeviceSetPalette(Framebuffer,Palette);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end; 
end;

{==============================================================================}

function FramebufferDeviceSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
{Set the brightness of the backlight of a framebuffer device}
{Framebuffer: The framebuffer device to set the backlight}
{Brightness: The brightness value to set (Normally 0 to 100)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Not all framebuffer devices support set backlight, returns ERROR_CALL_NOT_IMPLEMENTED if not supported}
{      Devices that support set backlight should set the flag FRAMEBUFFER_FLAG_BACKLIGHT}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Set Backlight (Brightness=' + IntToStr(Brightness) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceSetBacklight) then
  begin
   Result:=Framebuffer.DeviceSetBacklight(Framebuffer,Brightness);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end; 
end;

{==============================================================================}

function FramebufferDeviceGetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Get the current properties from a framebuffer device}
{Framebuffer: The framebuffer device to get properties from}
{Properties: Pointer to a TFramebufferProperties structure to return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Get Properties');
 {$ENDIF}

 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if Assigned(Framebuffer.DeviceGetProperties) then
  begin
   Result:=Framebuffer.DeviceGetProperties(Framebuffer,Properties);
  end
 else
  begin
   {Lock Framebuffer}
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   
   {Get Properties}
   Properties.Flags:=Framebuffer.Device.DeviceFlags;
   Properties.Address:=Framebuffer.Address;
   Properties.Size:=Framebuffer.Size;
   Properties.Pitch:=Framebuffer.Pitch;
   Properties.Depth:=Framebuffer.Depth;
   Properties.Order:=Framebuffer.Order;
   Properties.Mode:=Framebuffer.Mode;
   Properties.Format:=Framebuffer.Format;
   Properties.PhysicalWidth:=Framebuffer.PhysicalWidth;
   Properties.PhysicalHeight:=Framebuffer.PhysicalHeight;
   Properties.VirtualWidth:=Framebuffer.VirtualWidth;
   Properties.VirtualHeight:=Framebuffer.VirtualHeight;
   Properties.OffsetX:=Framebuffer.OffsetX;
   Properties.OffsetY:=Framebuffer.OffsetY;
   Properties.OverscanTop:=Framebuffer.OverscanTop;
   Properties.OverscanBottom:=Framebuffer.OverscanBottom;
   Properties.OverscanLeft:=Framebuffer.OverscanLeft;
   Properties.OverscanRight:=Framebuffer.OverscanRight;
   Properties.Rotation:=Framebuffer.Rotation;
   
   {Return Result}
   Result:=ERROR_SUCCESS;
   
   {Unlock Framebuffer}
   MutexUnlock(Framebuffer.Lock);
  end;  
end;

{==============================================================================}

function FramebufferDeviceSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Set the current properties for a framebuffer device}
{Framebuffer: The framebuffer device to set properties for}
{Properties: Pointer to a TFramebufferProperties structure containing the properties}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Changing certain properties may cause the framebuffer to be reallocated}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF DEVICE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Set Properties');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if Assigned(Framebuffer.DeviceSetProperties) then
  begin
   Result:=Framebuffer.DeviceSetProperties(Framebuffer,Properties);
  end;
end;

{==============================================================================}

function FramebufferDeviceCheckFlag(Framebuffer:PFramebufferDevice;Flag:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Check Enabled}
 {if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;

 {Check Flag}
 Result:=((Framebuffer.Device.DeviceFlags and Flag) <> 0);
 
 MutexUnlock(Framebuffer.Lock);
end;

{==============================================================================}

function FramebufferDeviceGetFormat(Framebuffer:PFramebufferDevice):LongWord;
begin
 {}
 Result:=COLOR_FORMAT_UNKNOWN;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Check Enabled}
 {if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get Format}
 Result:=Framebuffer.Format;
 
 MutexUnlock(Framebuffer.Lock);
end;

{==============================================================================}

function FramebufferDeviceCreate:PFramebufferDevice;
{Create a new Framebuffer entry}
{Return: Pointer to new Framebuffer entry or nil if Framebuffer could not be created}
begin
 {}
 Result:=FramebufferDeviceCreateEx(SizeOf(TFramebufferDevice));
end;

{==============================================================================}

function FramebufferDeviceCreateEx(Size:LongWord):PFramebufferDevice;
{Create a new Framebuffer entry}
{Size: Size in bytes to allocate for new Framebuffer (Including the Framebuffer entry)}
{Return: Pointer to new Framebuffer entry or nil if Framebuffer could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TFramebufferDevice) then Exit;
 
 {Create Framebuffer}
 Result:=PFramebufferDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=FRAMEBUFFER_TYPE_NONE;
 Result.Device.DeviceFlags:=FRAMEBUFFER_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Framebuffer}
 Result.FramebufferId:=DEVICE_ID_ANY;
 Result.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
 Result.DeviceAllocate:=nil;
 Result.DeviceRelease:=nil;
 Result.DeviceBlank:=nil;
 Result.DeviceRead:=nil;
 Result.DeviceWrite:=nil;
 Result.DeviceMark:=nil;
 Result.DeviceCommit:=nil;
 Result.DeviceGetRect:=nil;
 Result.DevicePutRect:=nil;
 Result.DeviceCopyRect:=nil;
 Result.DeviceFillRect:=nil;
 Result.DeviceGetLine:=nil;
 Result.DeviceGetPoint:=nil;
 Result.DeviceWaitSync:=nil;
 Result.DeviceGetOffset:=nil;
 Result.DeviceSetOffset:=nil;
 Result.DeviceGetPalette:=nil;
 Result.DeviceSetPalette:=nil;
 Result.DeviceSetBacklight:=nil;
 Result.DeviceGetProperties:=nil;
 Result.DeviceSetProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Address:=0;
 Result.Size:=0;
 Result.Pitch:=0;
 Result.Depth:=FRAMEBUFFER_DEPTH_32;
 Result.Order:=FRAMEBUFFER_ORDER_RGB;
 Result.Mode:=FRAMEBUFFER_MODE_ENABLED;
 Result.Format:=COLOR_FORMAT_DEFAULT;
 Result.PhysicalWidth:=0;
 Result.PhysicalHeight:=0;
 Result.VirtualWidth:=0;
 Result.VirtualHeight:=0;
 Result.OffsetX:=0;
 Result.OffsetY:=0;
 Result.OverscanTop:=0;    
 Result.OverscanBottom:=0; 
 Result.OverscanLeft:=0;  
 Result.OverscanRight:=0;  
 Result.Rotation:=FRAMEBUFFER_ROTATION_0;
 
 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to create lock for framebuffer device');
   FramebufferDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function FramebufferDeviceDestroy(Framebuffer:PFramebufferDevice):LongWord;
{Destroy an existing Framebuffer entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Framebuffer}
 Result:=ERROR_IN_USE;
 if FramebufferDeviceCheck(Framebuffer) = Framebuffer then Exit;

 {Check State}
 if Framebuffer.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Lock}
 if Framebuffer.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Framebuffer.Lock);
  end;
 
 {Destroy Framebuffer} 
 Result:=DeviceDestroy(@Framebuffer.Device);
end;

{==============================================================================}

function FramebufferDeviceRegister(Framebuffer:PFramebufferDevice):LongWord;
{Register a new Framebuffer in the Framebuffer table}
var
 FramebufferId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.FramebufferId <> DEVICE_ID_ANY then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Interfaces}
 if not(Assigned(Framebuffer.DeviceAllocate)) then Exit;
 if not(Assigned(Framebuffer.DeviceRelease)) then Exit;
 if not(Assigned(Framebuffer.DeviceSetProperties)) then Exit;
 
 {Check Framebuffer}
 Result:=ERROR_ALREADY_EXISTS;
 if FramebufferDeviceCheck(Framebuffer) = Framebuffer then Exit;
 
 {Check State}
 if Framebuffer.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert Framebuffer}
 if CriticalSectionLock(FramebufferDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Framebuffer}
    FramebufferId:=0;
    while FramebufferDeviceFind(FramebufferId) <> nil do
     begin
      Inc(FramebufferId);
     end;
    Framebuffer.FramebufferId:=FramebufferId;
    
    {Update Device}
    Framebuffer.Device.DeviceName:=FRAMEBUFFER_NAME_PREFIX + IntToStr(Framebuffer.FramebufferId);
    Framebuffer.Device.DeviceClass:=DEVICE_CLASS_FRAMEBUFFER;
    
    {Register Device}
    Result:=DeviceRegister(@Framebuffer.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Framebuffer.FramebufferId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link Framebuffer}
    if FramebufferDeviceTable = nil then
     begin
      FramebufferDeviceTable:=Framebuffer;
     end
    else
     begin
      Framebuffer.Next:=FramebufferDeviceTable;
      FramebufferDeviceTable.Prev:=Framebuffer;
      FramebufferDeviceTable:=Framebuffer;
     end;
 
    {Increment Count}
    Inc(FramebufferDeviceTableCount);
    
    {Check Default}
    if FramebufferDeviceDefault = nil then
     begin
      FramebufferDeviceDefault:=Framebuffer;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(FramebufferDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function FramebufferDeviceDeregister(Framebuffer:PFramebufferDevice):LongWord;
{Deregister a Framebuffer from the Framebuffer table}
var
 Prev:PFramebufferDevice;
 Next:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.FramebufferId = DEVICE_ID_ANY then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Framebuffer}
 Result:=ERROR_NOT_FOUND;
 if FramebufferDeviceCheck(Framebuffer) <> Framebuffer then Exit;
 
 {Check State}
 if Framebuffer.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove Framebuffer}
 if CriticalSectionLock(FramebufferDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Framebuffer.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink Framebuffer}
    Prev:=Framebuffer.Prev;
    Next:=Framebuffer.Next;
    if Prev = nil then
     begin
      FramebufferDeviceTable:=Next;
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
    Dec(FramebufferDeviceTableCount);
 
    {Check Default}
    if FramebufferDeviceDefault = Framebuffer then
     begin
      FramebufferDeviceDefault:=FramebufferDeviceTable;
     end;
     
    {Update Framebuffer}
    Framebuffer.FramebufferId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(FramebufferDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function FramebufferDeviceFind(FramebufferId:LongWord):PFramebufferDevice;
var
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if FramebufferId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(FramebufferDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Framebuffer}
    Framebuffer:=FramebufferDeviceTable;
    while Framebuffer <> nil do
     begin
      {Check State}
      if Framebuffer.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Framebuffer.FramebufferId = FramebufferId then
         begin
          Result:=Framebuffer;
          Exit;
         end;
       end;

      {Get Next}
      Framebuffer:=Framebuffer.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(FramebufferDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function FramebufferDeviceFindByName(const Name:String):PFramebufferDevice; inline;
begin
 {}
 Result:=PFramebufferDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function FramebufferDeviceFindByDescription(const Description:String):PFramebufferDevice; inline;
begin
 {}
 Result:=PFramebufferDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function FramebufferDeviceEnumerate(Callback:TFramebufferEnumerate;Data:Pointer):LongWord;
var
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(FramebufferDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Framebuffer}
    Framebuffer:=FramebufferDeviceTable;
    while Framebuffer <> nil do
     begin
      {Check State}
      if Framebuffer.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Framebuffer,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Framebuffer:=Framebuffer.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(FramebufferDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function FramebufferDeviceNotification(Framebuffer:PFramebufferDevice;Callback:TFramebufferNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_FRAMEBUFFER,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check Framebuffer}
   if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Framebuffer.Device,DEVICE_CLASS_FRAMEBUFFER,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{RTL Framebuffer Functions}
function SysFramebufferAvailable:Boolean;
{Check if a framebuffer device is available}
begin
 {}
 Result:=(FramebufferDeviceDefault <> nil);
end;

{==============================================================================}
{==============================================================================}
{Framebuffer Helper Functions}
function FramebufferDeviceGetCount:LongWord; inline;
{Get the current framebuffer device count}
begin
 {}
 Result:=FramebufferDeviceTableCount;
end;

{==============================================================================}

function FramebufferDeviceGetDefault:PFramebufferDevice; inline;
{Get the current default framebuffer device}
begin
 {}
 Result:=FramebufferDeviceDefault;
end;

{==============================================================================}

function FramebufferDeviceSetDefault(Framebuffer:PFramebufferDevice):LongWord; 
{Set the current default framebuffer device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(FramebufferDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Framebuffer}
    if FramebufferDeviceCheck(Framebuffer) <> Framebuffer then Exit;
    
    {Set Framebuffer Default}
    FramebufferDeviceDefault:=Framebuffer;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(FramebufferDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferDeviceCheck(Framebuffer:PFramebufferDevice):PFramebufferDevice;
{Check if the supplied Framebuffer device is in the Framebuffer table}
var
 Current:PFramebufferDevice;
begin
 {}
 Result:=nil;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(FramebufferDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Framebuffer}
    Current:=FramebufferDeviceTable;
    while Current <> nil do
     begin
      {Check Framebuffer}
      if Current = Framebuffer then
       begin
        Result:=Framebuffer;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(FramebufferDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function FramebufferDeviceSwap(Value:LongWord):LongWord; inline;
{No longer required (See ColorDefaultToFormat and ColorFormatToDefault)}
begin
 {}
 Result:=(Value and $FF00FF00) or ((Value and $00FF0000) shr 16) or ((Value and $000000FF) shl 16);
end;

{==============================================================================}

function FramebufferDepthToString(Depth:LongWord):String;
begin
 {}
 Result:='FRAMEBUFFER_DEPTH_UNKNOWN';
 
 case Depth of
  FRAMEBUFFER_DEPTH_8:Result:='FRAMEBUFFER_DEPTH_8';
  FRAMEBUFFER_DEPTH_16:Result:='FRAMEBUFFER_DEPTH_16';
  FRAMEBUFFER_DEPTH_24:Result:='FRAMEBUFFER_DEPTH_24';
  FRAMEBUFFER_DEPTH_32:Result:='FRAMEBUFFER_DEPTH_32';
 end;
end;

{==============================================================================}

function FramebufferOrderToString(Order:LongWord):String;
begin
 {}
 Result:='FRAMEBUFFER_ORDER_BGR_UNKNOWN';
 
 case Order of
  FRAMEBUFFER_ORDER_BGR:Result:='FRAMEBUFFER_ORDER_BGR';
  FRAMEBUFFER_ORDER_RGB:Result:='FRAMEBUFFER_ORDER_RGB';
 end;
end;

{==============================================================================}

function FramebufferModeToString(Mode:LongWord):String;
begin
 {}
 Result:='FRAMEBUFFER_MODE_UNKNOWN';
 
 case Mode of
  FRAMEBUFFER_MODE_ENABLED:Result:='FRAMEBUFFER_MODE_ENABLED';
  FRAMEBUFFER_MODE_REVERSED:Result:='FRAMEBUFFER_MODE_REVERSED';
  FRAMEBUFFER_MODE_IGNORED:Result:='FRAMEBUFFER_MODE_IGNORED';
 end;
end;

{==============================================================================}

function FramebufferRotationToString(Rotation:LongWord):String;
begin
 {}
 Result:='FRAMEBUFFER_ROTATION_UNKNOWN';
 
 case Rotation of
  FRAMEBUFFER_ROTATION_0:Result:='FRAMEBUFFER_ROTATION_0';
  FRAMEBUFFER_ROTATION_90:Result:='FRAMEBUFFER_ROTATION_90';
  FRAMEBUFFER_ROTATION_180:Result:='FRAMEBUFFER_ROTATION_180';
  FRAMEBUFFER_ROTATION_270:Result:='FRAMEBUFFER_ROTATION_270';
 end;
end;

{==============================================================================}
{==============================================================================}

initialization
 FramebufferInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
