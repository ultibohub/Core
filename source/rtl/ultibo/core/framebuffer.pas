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
 FRAMEBUFFER_FLAG_CACHED    = $00000001;   {If set framebuffer is in cached memory and cache cleaning should be used}
 FRAMEBUFFER_FLAG_SWAP      = $00000002;   {If set framebuffer uses reversed colors (BGR instead of RGB)}
 
{==============================================================================}
type
 {Framebuffer specific types}
 PFramebufferProperties = ^TFramebufferProperties;
 TFramebufferProperties = record
  Address:LongWord;                              {Framebuffer address (Ignored for Allocate / SetProperties)}
  Size:LongWord;                                 {Framebuffer size (Bytes) (Ignored for Allocate / SetProperties)}
  Pitch:LongWord;                                {Framebuffer pitch (Bytes per Line) (Ignored for Allocate / SetProperties)}
  Depth:LongWord;                                {Framebuffer depth (Bits per Pixel)(8/16/24/32)}
  Order:LongWord;                                {Framebuffer pixel order (BGR/RGB)}
  Mode:LongWord;                                 {Framebuffer alpha mode (Enabled/Reversed/Ignored)}
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
 end;
 
 PFramebufferDevice = ^TFramebufferDevice;
 
 {Framebuffer Enumeration Callback}
 TFramebufferEnumerate = function(Framebuffer:PFramebufferDevice;Data:Pointer):LongWord;
 {Framebuffer Notification Callback}
 TFramebufferNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {Framebuffer Device Methods}
 TFramebufferDeviceAllocate = function(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
 TFramebufferDeviceRelease = function(Framebuffer:PFramebufferDevice):LongWord;
 TFramebufferDeviceGetProperties = function(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
 TFramebufferDeviceSetProperties = function(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
 
 {Framebuffer Device}
 TFramebufferDevice = record
  {Device Properties}
  Device:TDevice;                                {The Device entry for this Framebuffer device}
  {Framebuffer Properties}
  FramebufferId:LongWord;                        {Unique Id of this Framebuffer device in the Framebuffer device table}
  FramebufferState:LongWord;                     {Framebuffer device state (eg FRAMEBUFFER_STATE_ENABLED)}
  DeviceAllocate:TFramebufferDeviceAllocate;     {A device specific DeviceAllocate method implementing a standard framebuffer device interface}
  DeviceRelease:TFramebufferDeviceRelease;       {A device specific DeviceRelease method implementing a standard framebuffer device interface}
  DeviceGetProperties:TFramebufferDeviceGetProperties; {A device specific DeviceGetProperties method implementing a standard framebuffer device interface (Or nil if the default method is suitable)}
  DeviceSetProperties:TFramebufferDeviceSetProperties; {A device specific DeviceSetProperties method implementing a standard framebuffer device interface}
  {Statistics Properties}
  AllocateCount:LongWord;
  ReleaseCount:LongWord;
  {Driver Properties}
  Lock:TMutexHandle;                             {Device lock}
  Address:LongWord;                              {Framebuffer address}
  Size:LongWord;                                 {Framebuffer size (Bytes)}
  Pitch:LongWord;                                {Framebuffer pitch (Bytes per Line)}
  Depth:LongWord;                                {Framebuffer depth (Bits per Pixel)(8/16/24/32)}
  Order:LongWord;                                {Framebuffer pixel order (BGR/RGB)}
  Mode:LongWord;                                 {Framebuffer alpha mode (Enabled/Reversed/Ignored)}
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
function FramebufferDeviceGetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function FramebufferDeviceSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;

function FramebufferDeviceCreate:PFramebufferDevice;
function FramebufferDeviceCreateEx(Size:LongWord):PFramebufferDevice;
function FramebufferDeviceDestroy(Framebuffer:PFramebufferDevice):LongWord;

function FramebufferDeviceRegister(Framebuffer:PFramebufferDevice):LongWord;
function FramebufferDeviceDeregister(Framebuffer:PFramebufferDevice):LongWord;

function FramebufferDeviceFind(FramebufferId:LongWord):PFramebufferDevice;
function FramebufferDeviceEnumerate(Callback:TFramebufferEnumerate;Data:Pointer):LongWord;

function FramebufferDeviceNotification(Framebuffer:PFramebufferDevice;Callback:TFramebufferNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{Framebuffer Helper Functions}
function FramebufferDeviceGetCount:LongWord; inline;
function FramebufferDeviceGetDefault:PFramebufferDevice; inline;

function FramebufferDeviceCheck(Framebuffer:PFramebufferDevice):PFramebufferDevice;

function FramebufferDeviceSwap(Value:LongWord):LongWord;

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
  
 {Setup Framebuffer Width / Height}
 if (FRAMEBUFFER_DEFAULT_WIDTH = 0) or (FRAMEBUFFER_DEFAULT_HEIGHT = 0) then
  begin
   {Get Dimensions Width and Height}
   Status:=FramebufferGetDimensions(FRAMEBUFFER_DEFAULT_WIDTH,FRAMEBUFFER_DEFAULT_HEIGHT,FRAMEBUFFER_DEFAULT_OVERSCAN_TOP,FRAMEBUFFER_DEFAULT_OVERSCAN_BOTTOM,FRAMEBUFFER_DEFAULT_OVERSCAN_LEFT,FRAMEBUFFER_DEFAULT_OVERSCAN_RIGHT);
   if Status <> ERROR_SUCCESS then
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'FramebufferGetDimensions failed: ' + ErrorToString(Status));
     
     {Set Dimension Defaults}
     FRAMEBUFFER_DEFAULT_WIDTH:=640;
     FRAMEBUFFER_DEFAULT_HEIGHT:=480;
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
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FramebufferDeviceGetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
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
   if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   
   {Get Properties}
   Properties.Address:=Framebuffer.Address;
   Properties.Size:=Framebuffer.Size;
   Properties.Pitch:=Framebuffer.Pitch;
   Properties.Depth:=Framebuffer.Depth;
   Properties.Order:=Framebuffer.Order;
   Properties.Mode:=Framebuffer.Mode;
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
   
   {Return Result}
   Result:=ERROR_SUCCESS;
   
   MutexUnlock(Framebuffer.Lock);
  end;  
end;

{==============================================================================}

function FramebufferDeviceSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
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
 Result:=ERROR_NOT_SUPPORTED;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;
 
 if Assigned(Framebuffer.DeviceSetProperties) then
  begin
   Result:=Framebuffer.DeviceSetProperties(Framebuffer,Properties);
  end;
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
 Result.DeviceGetProperties:=nil;
 Result.DeviceSetProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Address:=0;
 Result.Size:=0;
 Result.Pitch:=0;
 Result.Depth:=FRAMEBUFFER_DEPTH_32;
 Result.Order:=FRAMEBUFFER_ORDER_RGB;
 Result.Mode:=FRAMEBUFFER_MODE_ENABLED;
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
 
 {Create Lock}
 Result.Lock:=MutexCreate;
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

function FramebufferDeviceSwap(Value:LongWord):LongWord;
begin
 {}
 Result:=(Value and $FF00FF00) or ((Value and $00FF0000) shr 16) or ((Value and $000000FF) shl 16);
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
