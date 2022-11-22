{
Ultibo Framebuffer interface unit.

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
 
 FRAMEBUFFER_TYPE_MAX         = 2;
  
 {Framebuffer Type Names}
 FRAMEBUFFER_TYPE_NAMES:array[FRAMEBUFFER_TYPE_NONE..FRAMEBUFFER_TYPE_MAX] of String = (
  'FRAMEBUFFER_TYPE_NONE',
  'FRAMEBUFFER_TYPE_HARDWARE',
  'FRAMEBUFFER_TYPE_VIRTUAL');
 
 {Framebuffer Device States}
 FRAMEBUFFER_STATE_DISABLED   = 0;
 FRAMEBUFFER_STATE_ENABLED    = 1;

 FRAMEBUFFER_STATE_MAX        = 1;
 
 {Framebuffer State Names}
 FRAMEBUFFER_STATE_NAMES:array[FRAMEBUFFER_STATE_DISABLED..FRAMEBUFFER_STATE_MAX] of String = (
  'FRAMEBUFFER_STATE_DISABLED',
  'FRAMEBUFFER_STATE_ENABLED');

 {Framebuffer Cursor States}
 FRAMEBUFFER_CURSOR_DISABLED  = 0;
 FRAMEBUFFER_CURSOR_ENABLED   = 1;
 
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
 FRAMEBUFFER_FLAG_CURSOR    = $00000800;  {If set the framebuffer supports a hardware mouse cursor}
 
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
  Address:PtrUInt;                               {Framebuffer address (Ignored for Allocate / SetProperties)}
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
  CursorX:LongWord;                              {Framebuffer Cursor X (Pixels) (Ignored for Allocate / SetProperties)}
  CursorY:LongWord;                              {Framebuffer Cursor Y (Pixels) (Ignored for Allocate / SetProperties)}
  CursorState:LongWord;                          {Framebuffer Cursor State (eg FRAMEBUFFER_CURSOR_ENABLED) (Ignored for Allocate / SetProperties)}  
 end;
 
 PFramebufferDevice = ^TFramebufferDevice;
 
 {Framebuffer Enumeration Callback}
 TFramebufferEnumerate = function(Framebuffer:PFramebufferDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {Framebuffer Notification Callback}
 TFramebufferNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 {Framebuffer Device Methods}
 TFramebufferDeviceAllocate = function(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TFramebufferDeviceRelease = function(Framebuffer:PFramebufferDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TFramebufferDeviceBlank = function(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TFramebufferDeviceRead = function(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Len,Flags:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TFramebufferDeviceWrite = function(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Len,Flags:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TFramebufferDeviceMark = function(Framebuffer:PFramebufferDevice;X,Y,Width,Height,Flags:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TFramebufferDeviceCommit = function(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TFramebufferDeviceGetRect = function(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Skip,Flags:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TFramebufferDevicePutRect = function(Framebuffer:PFramebufferDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Skip,Flags:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TFramebufferDeviceCopyRect = function(Framebuffer:PFramebufferDevice;X1,Y1,X2,Y2,Width,Height,Flags:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TFramebufferDeviceFillRect = function(Framebuffer:PFramebufferDevice;X,Y,Width,Height,Color,Flags:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TFramebufferDeviceGetLine = function(Framebuffer:PFramebufferDevice;Y:LongWord):Pointer;{$IFDEF i386} stdcall;{$ENDIF}
 TFramebufferDeviceGetPoint = function(Framebuffer:PFramebufferDevice;X,Y:LongWord):Pointer;{$IFDEF i386} stdcall;{$ENDIF}
 
 TFramebufferDeviceWaitSync = function(Framebuffer:PFramebufferDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TFramebufferDeviceGetOffset = function(Framebuffer:PFramebufferDevice;var X,Y:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TFramebufferDeviceSetOffset = function(Framebuffer:PFramebufferDevice;X,Y:LongWord;Pan:Boolean):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TFramebufferDeviceGetPalette = function(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TFramebufferDeviceSetPalette = function(Framebuffer:PFramebufferDevice;Palette:PFramebufferPalette):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TFramebufferDeviceSetBacklight = function(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TFramebufferDeviceSetCursor = function(Framebuffer:PFramebufferDevice;Width,Height,HotspotX,HotspotY:LongWord;Image:Pointer;Len:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TFramebufferDeviceUpdateCursor = function(Framebuffer:PFramebufferDevice;Enabled:Boolean;X,Y:LongInt;Relative:Boolean):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TFramebufferDeviceGetProperties = function(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TFramebufferDeviceSetProperties = function(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
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
  DeviceSetCursor:TFramebufferDeviceSetCursor;         {A device specific DeviceSetCursor method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
  DeviceUpdateCursor:TFramebufferDeviceUpdateCursor;   {A device specific DeviceUpdateCursor method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
  DeviceGetProperties:TFramebufferDeviceGetProperties; {A device specific DeviceGetProperties method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
  DeviceSetProperties:TFramebufferDeviceSetProperties; {A device specific DeviceSetProperties method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
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
  Address:PtrUInt;                               {Framebuffer address}
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
  CursorX:LongWord;                              {Framebuffer Cursor X (Pixels)}
  CursorY:LongWord;                              {Framebuffer Cursor Y (Pixels)}
  CursorState:LongWord;                          {Framebuffer Cursor State (eg FRAMEBUFFER_CURSOR_ENABLED)}  
  {Buffer Properties}
  LineBuffer:Pointer;                            {Buffer for line fills}
  CopyBuffer:Pointer;                            {Buffer for overlapped copy}
  {Cursor Properties}
  CursorUpdate:LongBool;                         {Flag to indicate if cursor update (Show/Hide) is in progress}
  CursorImage:Pointer;                           {Buffer for cursor image pixels (COLOR_FORMAT_DEFAULT)}
  CursorInput:Pointer;                           {Buffer for cursor image pixels (Native color format)}
  CursorBuffer:Pointer;                          {Buffer for pixels currently under cursor (Native color format)}
  CursorOutput:Pointer;                          {Buffer for cursor pixels currently displayed (Native color format)}
  CursorWidth:LongWord;                          {Framebuffer Cursor Width (Pixels)}
  CursorHeight:LongWord;                         {Framebuffer Cursor Height (Pixels)}
  CursorHotspotX:LongWord;                       {Framebuffer Cursor Hotspot X (Pixels)}
  CursorHotspotY:LongWord;                       {Framebuffer Cursor Hotspot Y (Pixels)}
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
function FramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;
 
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

function FramebufferDeviceSetCursor(Framebuffer:PFramebufferDevice;Width,Height,HotspotX,HotspotY:LongWord;Image:Pointer;Len:LongWord):LongWord;
function FramebufferDeviceUpdateCursor(Framebuffer:PFramebufferDevice;Enabled:Boolean;X,Y:LongInt;Relative:Boolean):LongWord;

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

function FramebufferTypeToString(FramebufferType:LongWord):String;
function FramebufferStateToString(FramebufferState:LongWord):String;

procedure FramebufferDeviceHideCursor(Framebuffer:PFramebufferDevice);
procedure FramebufferDeviceShowCursor(Framebuffer:PFramebufferDevice);

function FramebufferCursorToString(State:LongWord):String;

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
{Initialize the Framebuffer unit and Framebuffer device table}

{Note: Called only during system startup}
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
 {Driver}
 Framebuffer.CursorX:=0;
 Framebuffer.CursorY:=0;
 Framebuffer.CursorState:=FRAMEBUFFER_CURSOR_DISABLED;
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
 {Cursor} 
 Framebuffer.CursorUpdate:=False;
 if Framebuffer.CursorImage <> nil then
  begin
   FreeMem(Framebuffer.CursorImage);
   Framebuffer.CursorImage:=nil;
  end;
 if Framebuffer.CursorInput <> nil then
  begin
   FreeMem(Framebuffer.CursorInput);
   Framebuffer.CursorInput:=nil;
  end;
 if Framebuffer.CursorBuffer <> nil then
  begin
   FreeMem(Framebuffer.CursorBuffer);
   Framebuffer.CursorBuffer:=nil;
  end;
 if Framebuffer.CursorOutput <> nil then
  begin
   FreeMem(Framebuffer.CursorOutput);
   Framebuffer.CursorOutput:=nil;
  end;
 Framebuffer.CursorWidth:=0;
 Framebuffer.CursorHeight:=0;
 Framebuffer.CursorHotspotX:=0;
 Framebuffer.CursorHotspotY:=0;
 
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
    
    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Hide Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceHideCursor(Framebuffer);
     end;
    
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
        CleanDataCacheRange(PtrUInt(Buffer),Size);
       end;

      {Create Data}
      {FillChar(Data,SizeOf(TDMAData),0);} {Not required}
      {$IFDEF CPU32}
      Data.SourceRange:=0;
      Data.DestRange:=0;
      {$ENDIF CPU32}
      Data.Source:=Pointer(Address);
      Data.Dest:=Buffer;
      Data.Flags:=DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_BULK;
      Data.StrideLength:=0;
      Data.SourceStride:=0;
      Data.DestStride:=0;
      Data.Size:=Size;
      Data.Next:=nil;
      
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
    
    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Show Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceShowCursor(Framebuffer);
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
 Height:LongWord;
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
    
    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Hide Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceHideCursor(Framebuffer);
     end;
    
    {Check DMA Available}
    if not(DMAAvailable) or not(SysInitCompleted) then
     begin
      Flags:=Flags and not(FRAMEBUFFER_TRANSFER_DMA);
     end;
    
    {Check DMA Transfer}
    if ((Flags and FRAMEBUFFER_TRANSFER_DMA) <> 0) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) then
     begin
      {Create Data}
      {FillChar(Data,SizeOf(TDMAData),0);} {Not required}
      {$IFDEF CPU32}
      Data.SourceRange:=0;
      Data.DestRange:=0;
      {$ENDIF CPU32}
      Data.Source:=Buffer;
      Data.Dest:=Pointer(Address);
      Data.Flags:=DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
      Data.StrideLength:=0;
      Data.SourceStride:=0;
      Data.DestStride:=0;
      Data.Size:=Size;
      Data.Next:=nil;
      
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
      Height:=Size div Framebuffer.Pitch;
      if (Height * Framebuffer.Pitch) < Size then Inc(Height);
      
      Framebuffer.DeviceMark(Framebuffer,0,Y,Framebuffer.VirtualWidth,Height,Flags);
     end;
   
    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Show Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceShowCursor(Framebuffer);
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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

function FramebufferDeviceCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Commit (Address=' + AddrToHex(Address) + ' Size=' + IntToStr(Size) + ')');
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
    
    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Hide Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceHideCursor(Framebuffer);
     end;
    
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
        CleanDataCacheRange(PtrUInt(Buffer),(Size + Skip) * Height);
       end;

      {Create Data}
      {FillChar(Data,SizeOf(TDMAData),0);} {Not required}
      {$IFDEF CPU32}
      Data.SourceRange:=0;
      Data.DestRange:=0;
      {$ENDIF CPU32}
      Data.Source:=Pointer(Address);
      Data.Dest:=Buffer;
      Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_BULK;
      Data.StrideLength:=Size;
      Data.SourceStride:=Stride;
      Data.DestStride:=Skip;
      Data.Size:=Data.StrideLength * Height;
      Data.Next:=nil;
      
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
   
    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Show Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceShowCursor(Framebuffer);
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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

    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Hide Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceHideCursor(Framebuffer);
     end;
    
    {Check DMA Available}
    if not(DMAAvailable) or not(SysInitCompleted) then
     begin
      Flags:=Flags and not(FRAMEBUFFER_TRANSFER_DMA);
     end;
    
    {Check DMA Transfer}
    if ((Flags and FRAMEBUFFER_TRANSFER_DMA) <> 0) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) then
     begin
      {Create Data}
      {FillChar(Data,SizeOf(TDMAData),0);} {Not required}
      {$IFDEF CPU32}
      Data.SourceRange:=0;
      Data.DestRange:=0;
      {$ENDIF CPU32}
      Data.Source:=Buffer;
      Data.Dest:=Pointer(Address);
      Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
      Data.StrideLength:=Size;
      Data.SourceStride:=Skip;
      Data.DestStride:=Stride;
      Data.Size:=Data.StrideLength * Height;
      Data.Next:=nil;
      
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
   
    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Show Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceShowCursor(Framebuffer);
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
    
    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Hide Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceHideCursor(Framebuffer);
     end;
    
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
          CleanDataCacheRange(PtrUInt(Buffer),SIZE_64K);
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
          {FillChar(Next^,SizeOf(TDMAData),0);} {Not required}
          {$IFDEF CPU32}
          Next.SourceRange:=0;
          Next.DestRange:=0;
          {$ENDIF CPU32}
          Next.Source:=Pointer(Framebuffer.Address + ((Framebuffer.OffsetY + Count) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X1) * (Framebuffer.Depth shr 3)));
          Next.Dest:=Buffer;
          Next.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
          Next.StrideLength:=Size;
          Next.SourceStride:=Stride;
          Next.DestStride:=0; {No stride on Buffer}
          Next.Size:=Size * Lines;
          Next.Next:=PDMAData(PtrUInt(Next) + SizeOf(TDMAData));
         
          {Get Next}
          Next:=Next.Next;
          
          {Create Data (From Buffer)}
          {FillChar(Next^,SizeOf(TDMAData),0);} {Not required}
          {$IFDEF CPU32}
          Next.SourceRange:=0;
          Next.DestRange:=0;
          {$ENDIF CPU32}
          Next.Source:=Buffer;
          Next.Dest:=Pointer(Framebuffer.Address + ((Framebuffer.OffsetY + Count) * Framebuffer.Pitch) + ((Framebuffer.OffsetX + X2) * (Framebuffer.Depth shr 3)));
          Next.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
          Next.StrideLength:=Size;
          Next.SourceStride:=0; {No stride on Buffer}
          Next.DestStride:=Stride;
          Next.Size:=Size * Lines;
          Next.Next:=PDMAData(PtrUInt(Next) + SizeOf(TDMAData));
          
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
          {FillChar(Data,SizeOf(TDMAData),0);} {Not required}
          {$IFDEF CPU32}
          Data.SourceRange:=0;
          Data.DestRange:=0;
          {$ENDIF CPU32}
          Data.Source:=Pointer(Source);
          Data.Dest:=Pointer(Address);
          Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
          Data.StrideLength:=Size;
          Data.SourceStride:=Stride;
          Data.DestStride:=Stride;
          Data.Size:=Data.StrideLength * Height;
          Data.Next:=nil;
          
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
          {FillChar(Data,SizeOf(TDMAData),0);} {Not required}
          {$IFDEF CPU32}
          Data.SourceRange:=0;
          Data.DestRange:=0;
          {$ENDIF CPU32}
          Data.Source:=Pointer(Source);
          Data.Dest:=Pointer(Address);
          Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
          Data.StrideLength:=Size;
          Data.SourceStride:=Stride;
          Data.DestStride:=Stride;
          Data.Size:=Data.StrideLength * Height;
          Data.Next:=nil;
          
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
   
    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Show Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceShowCursor(Framebuffer);
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
 {if Color = COLOR_NONE then Exit;} {Color must be in the framebuffer native format}
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
    
    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Hide Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceHideCursor(Framebuffer);
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
        CleanDataCacheRange(PtrUInt(Buffer),Size);
       end;
      
      {Create Data}
      {FillChar(Data,SizeOf(TDMAData),0);} {Not required}
      {$IFDEF CPU32}
      Data.SourceRange:=0;
      Data.DestRange:=0;
      {$ENDIF CPU32}
      Data.Source:=Buffer;
      Data.Dest:=Pointer(Address);
      Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
      Data.StrideLength:=Size;
      Data.SourceStride:=-Size; {Negative Stride}
      Data.DestStride:=Stride;
      Data.Size:=Data.StrideLength * Height;
      Data.Next:=nil;
       
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
   
    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Show Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceShowCursor(Framebuffer);
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
{Y: The Y (Row) offset value in pixels returned from the device if successful}
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
{Y: The Y (Row) offset value in pixels to set}
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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

function FramebufferDeviceSetCursor(Framebuffer:PFramebufferDevice;Width,Height,HotspotX,HotspotY:LongWord;Image:Pointer;Len:LongWord):LongWord;
{Set the mouse cursor image and properties of a framebuffer device}
{Framebuffer: The framebuffer device to set the cursor}
{Width: The width of the cursor image in pixels}
{Height: The height of the cursor image in pixels}
{HotspotX: The X hotspot of the cursor image, where the cursor X location will be (Normally 0)}
{HotspotY: The Y hotspot of the cursor image, where the cursor Y location will be (Normally 0)}
{Image: A buffer containing the cursor image pixels in COLOR_FORMAT_DEFAULT format}
{Len: The length of the image buffer in bytes}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For devices that don't support hardware mouse cursor a software cursor will be implemented
       If image is nil then the default cursor image will be used}
var
 Size:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF FRAMEBUFFER_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Set Cursor (Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ' HotspotX=' + IntToStr(HotspotX) + ' HotspotY=' + IntToStr(HotspotY) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;

 if Assigned(Framebuffer.DeviceSetCursor) then
  begin
   Result:=Framebuffer.DeviceSetCursor(Framebuffer,Width,Height,HotspotX,HotspotY,Image,Len);
  end
 else
  begin
   {Lock Framebuffer}
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   try
    {Check Address}
    if Framebuffer.Address = 0 then Exit;
 
    {Check Image}
    if Image = nil then
     begin
      {Set Parameters}
      Width:=CURSOR_ARROW_DEFAULT_WIDTH;
      Height:=CURSOR_ARROW_DEFAULT_HEIGHT;
      HotspotX:=0;
      HotspotY:=0;
      Image:=@CURSOR_ARROW_DEFAULT;
      Len:=SizeOf(CURSOR_ARROW_DEFAULT);
     end;
    
    {Check Width and Height}
    if (Width = 0) or (Height = 0) then Exit;
    
    {Check Len}
    if Len < (Width * Height * ColorFormatToBytes(COLOR_FORMAT_DEFAULT)) then Exit;
    
    {Check Current Image}
    if Framebuffer.CursorImage <> nil then
     begin
      {Check Enabled}
      if Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED then
       begin
        {Hide Cursor}
        FramebufferDeviceHideCursor(Framebuffer);
       end;
      
      {Free Image}
      FreeMem(Framebuffer.CursorImage);

      {Free Input}
      FreeMem(Framebuffer.CursorInput);
      
      {Free Buffer}
      FreeMem(Framebuffer.CursorBuffer);
 
      {Free Output}
      FreeMem(Framebuffer.CursorOutput);
      
      {Reset Properties}
      Framebuffer.CursorUpdate:=False;
      Framebuffer.CursorImage:=nil;
      Framebuffer.CursorInput:=nil;
      Framebuffer.CursorBuffer:=nil;
      Framebuffer.CursorOutput:=nil;
      Framebuffer.CursorWidth:=0;
      Framebuffer.CursorHeight:=0;
      Framebuffer.CursorHotspotX:=0;
      Framebuffer.CursorHotspotY:=0;
     end;

    try
     Result:=ERROR_NOT_ENOUGH_MEMORY;
     
     {Get Image Size}
     Size:=(Width * Height * ColorFormatToBytes(COLOR_FORMAT_DEFAULT));
      
     {Allocate Image}
     Framebuffer.CursorImage:=GetMem(Size);
     if Framebuffer.CursorImage = nil then Exit;
     
     {Copy Image}
     System.Move(Image^,Framebuffer.CursorImage^,Size);
     
     {Get Input, Buffer and Output Size}
     Size:=(Width * Height * ColorFormatToBytes(Framebuffer.Format));
     
     {Allocate Input}
     Framebuffer.CursorInput:=GetMem(Size);
     if Framebuffer.CursorInput = nil then Exit;
     
     {Allocate Buffer}
     Framebuffer.CursorBuffer:=GetMem(Size);
     if Framebuffer.CursorBuffer = nil then Exit;
 
     {Allocate Output}
     Framebuffer.CursorOutput:=GetMem(Size);
     if Framebuffer.CursorOutput = nil then Exit;
     
     {Convert Input (COLOR_FORMAT_DEFAULT to Framebuffer format)}
     PixelsDefaultToFormat(Framebuffer.Format,Framebuffer.CursorImage,Framebuffer.CursorInput,Width * Height,(Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_SWAP) <> 0);
     
     {Set Properties}
     Framebuffer.CursorWidth:=Width;
     Framebuffer.CursorHeight:=Height;
     Framebuffer.CursorHotspotX:=HotspotX;
     Framebuffer.CursorHotspotY:=HotspotY;
     
     {Check Enabled}
     if Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED then
      begin
       {Show Cursor}
       FramebufferDeviceShowCursor(Framebuffer);
      end;
     
     {Return Result}
     Result:=ERROR_SUCCESS;
    finally
     if Result <> ERROR_SUCCESS then
      begin
       {Free Image}
       if Framebuffer.CursorImage <> nil then FreeMem(Framebuffer.CursorImage);
 
       {Free Input}
       if Framebuffer.CursorInput <> nil then FreeMem(Framebuffer.CursorInput);
       
       {Free Buffer}
       if Framebuffer.CursorBuffer <> nil then FreeMem(Framebuffer.CursorBuffer);
  
       {Free Output}
       if Framebuffer.CursorOutput <> nil then FreeMem(Framebuffer.CursorOutput);
       
       {Reset Properties}
       Framebuffer.CursorUpdate:=False;
       Framebuffer.CursorImage:=nil;
       Framebuffer.CursorInput:=nil;
       Framebuffer.CursorBuffer:=nil;
       Framebuffer.CursorOutput:=nil;
       Framebuffer.CursorWidth:=0;
       Framebuffer.CursorHeight:=0;
       Framebuffer.CursorHotspotX:=0;
       Framebuffer.CursorHotspotY:=0;
      end;
    end;
   finally
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
   end; 
  end;  
end;

{==============================================================================}

function FramebufferDeviceUpdateCursor(Framebuffer:PFramebufferDevice;Enabled:Boolean;X,Y:LongInt;Relative:Boolean):LongWord;
{Update the position and state for the mouse cursor of a framebuffer device}
{Framebuffer: The framebuffer device to update the cursor}
{Enabled: If true then show the cursor else hide it}
{X: The cursor X location in pixels}
{Y: The cursor Y location in pixels}
{Relative: If true then X and Y are considered relative to the current position}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For devices that don't support hardware mouse cursor a software cursor will be implemented}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF FRAMEBUFFER_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Update Cursor (Enabled=' + BoolToStr(Enabled) + ' X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Relative=' + BoolToStr(Relative) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;

 if Assigned(Framebuffer.DeviceUpdateCursor) then
  begin
   Result:=Framebuffer.DeviceUpdateCursor(Framebuffer,Enabled,X,Y,Relative);
  end
 else
  begin
   {Lock Framebuffer}
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   try
    {Check Address}
    if Framebuffer.Address = 0 then Exit;
 
    {Check State}
    if Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED then
     begin
      {Hide Cursor}
      FramebufferDeviceHideCursor(Framebuffer);
     end;

    {Set Position}
    if not Relative then
     begin
      Framebuffer.CursorX:=X;
      Framebuffer.CursorY:=Y;
     end
    else
     begin
      Framebuffer.CursorX:=Framebuffer.CursorX + X;
      Framebuffer.CursorY:=Framebuffer.CursorY + Y;
     end;
     
    {Check Enabled}
    if Enabled then
     begin
      {Set State} 
      Framebuffer.CursorState:=FRAMEBUFFER_CURSOR_ENABLED;
      
      {Show Cursor}
      FramebufferDeviceShowCursor(Framebuffer);
     end
    else
     begin
      {Set State} 
      Framebuffer.CursorState:=FRAMEBUFFER_CURSOR_DISABLED;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
   end; 
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
 
 {$IFDEF FRAMEBUFFER_DEBUG}
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
   Properties.CursorX:=Framebuffer.CursorX;
   Properties.CursorY:=Framebuffer.CursorY;
   Properties.CursorState:=Framebuffer.CursorState;
   
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
var
 Current:TFramebufferProperties;
 Updated:TFramebufferProperties;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF FRAMEBUFFER_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Framebuffer Device Set Properties');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if Assigned(Framebuffer.DeviceSetProperties) then
  begin
   Result:=Framebuffer.DeviceSetProperties(Framebuffer,Properties);
  end
 else
  begin
   {Lock Framebuffer}
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   try
    {Get Current Properties}
    Result:=FramebufferDeviceGetProperties(Framebuffer,@Current);
    if Result <> ERROR_SUCCESS then Exit;

    {Build Updated Properties}
    Updated:=Properties^;
    Updated.Flags:=Current.Flags;
    Updated.Address:=Current.Address;
    Updated.Size:=Current.Size;
    Updated.Pitch:=Current.Pitch;
    Updated.Format:=Current.Format;
    Updated.CursorX:=Current.CursorX;
    Updated.CursorY:=Current.CursorY;
    Updated.CursorState:=Current.CursorState;

    {Compare Properties}
    if not CompareMem(@Current,@Updated,SizeOf(TFramebufferProperties)) then
     begin
      {Release Framebuffer}
      Result:=FramebufferDeviceRelease(Framebuffer);
      if Result <> ERROR_SUCCESS then Exit;

      {Allocate Framebuffer}
      Result:=FramebufferDeviceAllocate(Framebuffer,@Updated);
      if Result <> ERROR_SUCCESS then Exit;
     end;
   finally
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
   end; 
  end;
end;

{==============================================================================}

function FramebufferDeviceCheckFlag(Framebuffer:PFramebufferDevice;Flag:LongWord):Boolean;
{Check if a framebuffer device supports a flag value}
{Framebuffer: The framebuffer device to check}
{Flag: The framebuffer flag to check (eg FRAMEBUFFER_FLAG_BACKLIGHT)}
{Return: True if flag is supported, False if not or on error}
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
{Get the color format of a framebuffer device}
{Framebuffer: The framebuffer device to get from}
{Return: The color format of the framebuffer (eg COLOR_FORMAT_RGB24)}
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
 Result.DeviceSetCursor:=nil;
 Result.DeviceUpdateCursor:=nil;
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
 Result.CursorX:=0;
 Result.CursorY:=0;
 Result.CursorState:=FRAMEBUFFER_CURSOR_DISABLED;
 Result.CursorWidth:=0;
 Result.CursorHeight:=0;
 Result.CursorHotspotX:=0;
 Result.CursorHotspotY:=0;
 
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
{Framebuffer: The framebuffer device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
{Framebuffer: The framebuffer device to register}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
{Framebuffer: The framebuffer device to deregister}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
{Find a framebuffer device by ID in the framebuffer table}
{FramebufferId: The ID number of the framebuffer to find}
{Return: Pointer to framebuffer device entry or nil if not found}
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
{Find a framebuffer device by name in the framebuffer table}
{Name: The name of the framebuffer to find (eg Framebuffer0)}
{Return: Pointer to framebuffer device entry or nil if not found}
begin
 {}
 Result:=PFramebufferDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function FramebufferDeviceFindByDescription(const Description:String):PFramebufferDevice; inline;
{Find a framebuffer device by description in the framebuffer table}
{Description: The description of the framebuffer to find (eg BCM2836 Framebuffer)}
{Return: Pointer to framebuffer device entry or nil if not found}
begin
 {}
 Result:=PFramebufferDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function FramebufferDeviceEnumerate(Callback:TFramebufferEnumerate;Data:Pointer):LongWord;
{Enumerate all framebuffer devices in the framebuffer table}
{Callback: The callback function to call for each framebuffer in the table}
{Data: A private data pointer to pass to callback for each framebuffer in the table}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
{Register a notification for framebuffer device changes}
{Framebuffer: The framebuffer device to notify changes for (Optional, pass nil for all framebuffer devices)}
{Callback: The function to call when a notification event occurs}
{Data: A private data pointer to pass to callback when a notification event occurs}
{Notification: The events to register for notification of (eg DEVICE_NOTIFICATION_REGISTER)}
{Flags: The flags to control the notification (eg NOTIFIER_FLAG_WORKER)}
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

function FramebufferTypeToString(FramebufferType:LongWord):String;
{Convert a Framebuffer type value to a string}
begin
 {}
 Result:='FRAMEBUFFER_TYPE_UNKNOWN';
 
 if FramebufferType <= FRAMEBUFFER_TYPE_MAX then
  begin
   Result:=FRAMEBUFFER_TYPE_NAMES[FramebufferType];
  end;
end;

{==============================================================================}

function FramebufferStateToString(FramebufferState:LongWord):String;
{Convert a Framebuffer state value to a string}
begin
 {}
 Result:='FRAMEBUFFER_STATE_UNKNOWN';
 
 if FramebufferState <= FRAMEBUFFER_STATE_MAX then
  begin
   Result:=FRAMEBUFFER_STATE_NAMES[FramebufferState];
  end;
end;

{==============================================================================}

procedure FramebufferDeviceHideCursor(Framebuffer:PFramebufferDevice);
{Restore the framebuffer area under the cursor from the cursor buffer}

{Note: Caller must hold the framebuffer lock}
{Note: Driver support function only, not intended for direct use by applications}
var
 X:LongWord;
 Y:LongWord;
 Skip:LongWord;
 Width:LongWord;
 Height:LongWord;
 Buffer:Pointer;
begin
 {}
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 
 {Check Update}
 if Framebuffer.CursorUpdate then Exit;
 
 {Check Buffer}
 if Framebuffer.CursorBuffer = nil then Exit;
 
 {Set Update}
 Framebuffer.CursorUpdate:=True;
 
 {Get Parameters}
 X:=Framebuffer.CursorX;
 Y:=Framebuffer.CursorY;
 Skip:=0;
 Width:=Framebuffer.CursorWidth;
 Height:=Framebuffer.CursorHeight;
 Buffer:=Framebuffer.CursorBuffer;

 {Update X Parameters}
 {Check X less then Hotspot X} 
 if Framebuffer.CursorX < Framebuffer.CursorHotspotX then
  begin
   {Start at 0}
   X:=0;
   Skip:=Framebuffer.CursorHotspotX;
   Dec(Width,Skip);
   Inc(Buffer,Skip * ColorFormatToBytes(Framebuffer.Format));
  end
 else
  begin
   {Subtract Hotspot X}
   Dec(X,Framebuffer.CursorHotspotX);
   
   {Check X greater than Physical Width}
   if X >= Framebuffer.PhysicalWidth then
    begin
     {Clear Update}
     Framebuffer.CursorUpdate:=False;
     Exit;
    end; 

   {Check X plus Width greater than Physical Width}   
   if X + (Width - 1) >= Framebuffer.PhysicalWidth then
    begin
     Skip:=(X + Width) - Framebuffer.PhysicalWidth;
     Dec(Width,Skip);
    end;
  end;

 {Update Y Parameters} 
 {Check Y less then Hotspot Y} 
 if Framebuffer.CursorY < Framebuffer.CursorHotspotY then
  begin
   {Start at 0}
   Y:=0;
   Dec(Height,Framebuffer.CursorHotspotY);
   Inc(Buffer,(Framebuffer.CursorHotspotY * (Width + Skip)) * ColorFormatToBytes(Framebuffer.Format));
  end
 else
  begin
   {Subtract Hotspot Y}
   Dec(Y,Framebuffer.CursorHotspotY);
   
   {Check Y greater than Physical Height}
   if Y >= Framebuffer.PhysicalHeight then
    begin
     {Clear Update}
     Framebuffer.CursorUpdate:=False;
     Exit;
    end; 

   {Check Y plus Height greater than Physical Height}   
   if Y + (Height - 1) >= Framebuffer.PhysicalHeight then
    begin
     Dec(Height,(Y + Height) - Framebuffer.PhysicalHeight);
    end;
  end;
 
 {Put Buffer}
 FramebufferDevicePutRect(Framebuffer,X,Y,Buffer,Width,Height,Skip,FRAMEBUFFER_TRANSFER_NONE);
 
 {Clear Update}
 Framebuffer.CursorUpdate:=False;
end;

{==============================================================================}

procedure FramebufferDeviceShowCursor(Framebuffer:PFramebufferDevice);
{Save the framebuffer area under the cursor to the cursor buffer and merge the
 cursor input with it to create the cursor output. Put the output to the framebuffer}

{Note: Caller must hold the framebuffer lock}
{Note: Driver support function only, not intended for direct use by applications}
var
 X:LongWord;
 Y:LongWord;
 Skip:LongWord;
 Width:LongWord;
 Height:LongWord;
 Offset:LongWord;
 SizeImage:LongWord;
 SizeBuffer:LongWord;
 OffsetImage:LongWord;
 OffsetBuffer:LongWord;
begin
 {}
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 
 {Check Update}
 if Framebuffer.CursorUpdate then Exit;
 
 {Check Image}
 if Framebuffer.CursorImage = nil then Exit;
 
 {Check Input}
 if Framebuffer.CursorInput = nil then Exit;
 
 {Check Buffer}
 if Framebuffer.CursorBuffer = nil then Exit;
 
 {Check Output}
 if Framebuffer.CursorOutput = nil then Exit;
 
 {Set Update}
 Framebuffer.CursorUpdate:=True;
 
 {Get Parameters}
 X:=Framebuffer.CursorX;
 Y:=Framebuffer.CursorY;
 Skip:=0;
 Width:=Framebuffer.CursorWidth;
 Height:=Framebuffer.CursorHeight;
 Offset:=0;

 {Update X Parameters}
 {Check X less then Hotspot X} 
 if Framebuffer.CursorX < Framebuffer.CursorHotspotX then
  begin
   {Start at 0}
   X:=0;
   Skip:=Framebuffer.CursorHotspotX;
   Dec(Width,Skip);
   Inc(Offset,Skip * ColorFormatToBytes(Framebuffer.Format));
  end
 else
  begin
   {Subtract Hotspot X}
   Dec(X,Framebuffer.CursorHotspotX);
   
   {Check X greater than Physical Width}
   if X >= Framebuffer.PhysicalWidth then
    begin
     {Clear Update}
     Framebuffer.CursorUpdate:=False;
     Exit;
    end; 

   {Check X plus Width greater than Physical Width}   
   if X + (Width - 1) >= Framebuffer.PhysicalWidth then
    begin
     Skip:=(X + Width) - Framebuffer.PhysicalWidth;
     Dec(Width,Skip);
    end;
  end;

 {Update Y Parameters} 
 {Check Y less then Hotspot Y} 
 if Framebuffer.CursorY < Framebuffer.CursorHotspotY then
  begin
   {Start at 0}
   Y:=0;
   Dec(Height,Framebuffer.CursorHotspotY);
   Inc(Offset,(Framebuffer.CursorHotspotY * (Width + Skip)) * ColorFormatToBytes(Framebuffer.Format));
  end
 else
  begin
   {Subtract Hotspot Y}
   Dec(Y,Framebuffer.CursorHotspotY);
   
   {Check Y greater than Physical Height}
   if Y >= Framebuffer.PhysicalHeight then
    begin
     {Clear Update}
     Framebuffer.CursorUpdate:=False;
     Exit;
    end; 

   {Check Y plus Height greater than Physical Height}   
   if Y + (Height - 1) >= Framebuffer.PhysicalHeight then
    begin
     Dec(Height,(Y + Height) - Framebuffer.PhysicalHeight);
    end;
  end;
 
 {Get Buffer}
 FramebufferDeviceGetRect(Framebuffer,X,Y,Pointer(Framebuffer.CursorBuffer + Offset),Width,Height,Skip,FRAMEBUFFER_TRANSFER_NONE);
 
 {Create Output}
 SizeImage:=Framebuffer.CursorWidth * Framebuffer.CursorHeight * ColorFormatToBytes(COLOR_FORMAT_DEFAULT);
 SizeBuffer:=Framebuffer.CursorWidth * Framebuffer.CursorHeight * ColorFormatToBytes(Framebuffer.Format);
 OffsetImage:=0;
 OffsetBuffer:=0;
 case Framebuffer.Depth of
  FRAMEBUFFER_DEPTH_8:begin
    while OffsetImage < SizeImage do
     begin
      if PLongWord(Framebuffer.CursorImage + OffsetImage)^ = COLOR_NONE then
       begin
        PByte(Framebuffer.CursorOutput + OffsetBuffer)^:=PByte(Framebuffer.CursorBuffer + OffsetBuffer)^;
       end
      else
       begin
        PByte(Framebuffer.CursorOutput + OffsetBuffer)^:=PByte(Framebuffer.CursorInput + OffsetBuffer)^;
       end;
       
      Inc(OffsetImage,4);
      Inc(OffsetBuffer,1);
     end;
   end;
  FRAMEBUFFER_DEPTH_16:begin
    while OffsetImage < SizeImage do
     begin
      if PLongWord(Framebuffer.CursorImage + OffsetImage)^ = COLOR_NONE then
       begin
        PWord(Framebuffer.CursorOutput + OffsetBuffer)^:=PWord(Framebuffer.CursorBuffer + OffsetBuffer)^;
       end
      else
       begin
        PWord(Framebuffer.CursorOutput + OffsetBuffer)^:=PWord(Framebuffer.CursorInput + OffsetBuffer)^;
       end;
       
      Inc(OffsetImage,4);
      Inc(OffsetBuffer,2);
     end;
   end;
  FRAMEBUFFER_DEPTH_24:begin
    while OffsetImage < SizeImage do
     begin
      if PLongWord(Framebuffer.CursorImage + OffsetImage)^ = COLOR_NONE then
       begin
        PWord(Framebuffer.CursorOutput + OffsetBuffer)^:=PWord(Framebuffer.CursorBuffer + OffsetBuffer)^;
        PByte(Framebuffer.CursorOutput + OffsetBuffer + 2)^:=PByte(Framebuffer.CursorBuffer + OffsetBuffer + 2)^;
       end
      else
       begin
        PWord(Framebuffer.CursorOutput + OffsetBuffer)^:=PWord(Framebuffer.CursorInput + OffsetBuffer)^;
        PByte(Framebuffer.CursorOutput + OffsetBuffer + 2)^:=PByte(Framebuffer.CursorInput + OffsetBuffer + 2)^;
       end;
       
      Inc(OffsetImage,4);
      Inc(OffsetBuffer,3);
     end;
   end;
  FRAMEBUFFER_DEPTH_32:begin
    while OffsetImage < SizeImage do
     begin
      if PLongWord(Framebuffer.CursorImage + OffsetImage)^ = COLOR_NONE then
       begin
        PLongWord(Framebuffer.CursorOutput + OffsetBuffer)^:=PLongWord(Framebuffer.CursorBuffer + OffsetBuffer)^;
       end
      else
       begin
        PLongWord(Framebuffer.CursorOutput + OffsetBuffer)^:=PLongWord(Framebuffer.CursorInput + OffsetBuffer)^;
       end;
       
      Inc(OffsetImage,4);
      Inc(OffsetBuffer,4);
     end;
   end;
 end;  
 
 {Put Output}
 FramebufferDevicePutRect(Framebuffer,X,Y,Pointer(Framebuffer.CursorOutput + Offset),Width,Height,Skip,FRAMEBUFFER_TRANSFER_NONE);
 
 {Clear Update}
 Framebuffer.CursorUpdate:=False;
end;

{==============================================================================}

function FramebufferCursorToString(State:LongWord):String;
begin
 {}
 Result:='FRAMEBUFFER_CURSOR_UNKNOWN';

 case State of
  FRAMEBUFFER_CURSOR_DISABLED:Result:='FRAMEBUFFER_CURSOR_DISABLED';
  FRAMEBUFFER_CURSOR_ENABLED:Result:='FRAMEBUFFER_CURSOR_ENABLED';
 end; 
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
