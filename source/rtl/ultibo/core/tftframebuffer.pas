{
Ultibo TFT Framebuffer driver library unit.

Copyright (C) 2018 - SoftOz Pty Ltd.

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

  Linux - \drivers\staging\fbtft\fbtft-core.c - Copyright (C) 2013 Noralf Tronnes
 
References
==========


TFT Framebuffer
===============

 This is a generic framebuffer device support unit for SPI based TFT screens using common chipsets.
 
 This unit implements the shared functionality that is common to all devices and provides a set of
 functions that device specific drivers must implement. For each supported device a driver unit is
 required that implements the SPI communications to initialize the device, setup resolution and color
 depth as well as refreshing the framebuffer data to the device memory on change.
  

 The resulting device created by the combination of this unit and a device specific driver is registered
 with Ultibo as a framebuffer device that can be accessed using all of the standard framebuffer API
 functions.
 
 For examples of drivers that use this support unit see the HX8357D and ILI9340 units.
  
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit TFTFramebuffer;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,HeapManager,Devices,DMA,GPIO,SPI,Framebuffer,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {TFTFramebuffer specific constants}
 TFT_FRAMEBUFFER_FRAME_RATE_DEFAULT = 20; {Default frame rate of 20 frames per second refresh}
 
{==============================================================================}
type
 {TFTFramebuffer specific types}
 PTFTFramebuffer = ^TTFTFramebuffer;
 
 {TFTFramebuffer Device Methods}
 TTFTFramebufferInitialize = function(Framebuffer:PTFTFramebuffer;Defaults:PFramebufferProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TTFTFramebufferDeinitialize = function(Framebuffer:PTFTFramebuffer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TTFTFramebufferGetDefaults = function(Framebuffer:PTFTFramebuffer;Properties,Defaults:PFramebufferProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TTFTFramebufferSetWriteAddress = function(Framebuffer:PTFTFramebuffer;X1,Y1,X2,Y2:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TTFTFramebufferWriteMemory = function(Framebuffer:PTFTFramebuffer;Address,Size:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 {TFTFramebuffer Device}
 TTFTFramebuffer = record
  {Framebuffer Properties}
  Framebuffer:TFramebufferDevice;
  {TFT Properties}
  SPI:PSPIDevice;                                 {The SPI device this Framebuffer is connected to}
  ChipSelect:Word;                                {The SPI chip select of the device}
  RST:TGPIOInfo;                                  {The GPIO information for the reset line}
  DC:TGPIOInfo;                                   {The GPIO information for the data/command line}
  BL:TGPIOInfo;                                   {The GPIO information for the backlight line}
  Initialize:TTFTFramebufferInitialize;           {A device specific Initialize method (Optional)}
  Deinitialize:TTFTFramebufferDeinitialize;       {A device specific Deinitialize method (Optional)}
  GetDefaults:TTFTFramebufferGetDefaults;         {A device specific GetDefaults method (Mandatory)}
  SetWriteAddress:TTFTFramebufferSetWriteAddress; {A device specific SetWriteAddress method (Mandatory)}
  WriteMemory:TTFTFramebufferWriteMemory;         {A device specific WriteMemory method (Mandatory)}
  {Driver Properties}
  Width:LongWord;                                 {Framebuffer Width in Pixels}
  Height:LongWord;                                {Framebuffer Height in Pixels}
  Rotation:LongWord;                              {Framebuffer Rotation (eg FRAMEBUFFER_ROTATION_180)}
  DirtyY1:LongWord;                               {First line of dirty region (or Height - 1 if none dirty)}
  DirtyY2:LongWord;                               {Last line of dirty region (or 0 if none dirty)}
  Ready:LongBool;                                 {If True timer should be enabled during Mark operation}
  Lock:TMutexHandle;                              {Lock for dirty region redraw}
  Timer:TTimerHandle;                             {Handle for dirty region redraw timer}
  FrameRate:LongWord;                             {Frame rate for display refresh (in Frames Per Second)}
  TransferSize:LongWord;                          {Maximum transfer size for the SPI device (or -1 if No Maximum)}
 end; 

{==============================================================================}
{var}
 {TFTFramebuffer specific variables}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{TFTFramebuffer Functions}
function TFTFramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function TFTFramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;

function TFTFramebufferMark(Framebuffer:PFramebufferDevice;X,Y,Width,Height,Flags:LongWord):LongWord;
function TFTFramebufferCommit(Framebuffer:PFramebufferDevice;Address,Size,Flags:LongWord):LongWord;

function TFTFramebufferSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;

procedure TFTFramebufferUpdateDisplay(Framebuffer:PTFTFramebuffer);

{==============================================================================}
{TFTFramebuffer Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {TFTFramebuffer specific variables}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{==============================================================================}
{TFTFramebuffer Functions}
function TFTFramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Implementation of FramebufferDeviceAllocate API for TFT Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceAllocate instead}
var
 Bytes:LongWord;
 Buffer:Pointer;
 Defaults:TFramebufferProperties;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF FRAMEBUFFER_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'TFT Framebuffer Allocate');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Parameters}
    if not(Assigned(PTFTFramebuffer(Framebuffer).GetDefaults)) then Exit;
    if not(Assigned(PTFTFramebuffer(Framebuffer).SetWriteAddress)) then Exit;
    if not(Assigned(PTFTFramebuffer(Framebuffer).WriteMemory)) then Exit;
    if PTFTFramebuffer(Framebuffer).TransferSize = 0 then Exit;
    
    {Get Defaults}
    if Assigned(PTFTFramebuffer(Framebuffer).GetDefaults) then
     begin
      Result:=PTFTFramebuffer(Framebuffer).GetDefaults(PTFTFramebuffer(Framebuffer),Properties,@Defaults);
      if Result <> ERROR_SUCCESS then Exit;
     end;
 
    {Get Bytes}
    Bytes:=ColorFormatToBytes(Defaults.Format);
    if Bytes = 0 then Exit;
    
    {Get Size}
    Defaults.Size:=(Defaults.PhysicalWidth * Defaults.PhysicalHeight) * Bytes;
    
    {Get Pitch}
    Defaults.Pitch:=Defaults.PhysicalWidth * Bytes;
    
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
      CleanDataCacheRange(LongWord(Buffer),Defaults.Size);
     end;
    
    {Initialize Framebuffer}
    if Assigned(PTFTFramebuffer(Framebuffer).Initialize) then
     begin
      Result:=PTFTFramebuffer(Framebuffer).Initialize(PTFTFramebuffer(Framebuffer),@Defaults);
      if Result <> ERROR_SUCCESS then Exit;
     end; 
    
    {Update Framebuffer}
    Framebuffer.Address:=LongWord(Buffer);
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
    PTFTFramebuffer(Framebuffer).DirtyY1:=Framebuffer.PhysicalHeight - 1;
    PTFTFramebuffer(Framebuffer).DirtyY2:=0;
    PTFTFramebuffer(Framebuffer).Ready:=True;
    PTFTFramebuffer(Framebuffer).Lock:=MutexCreate;
    if PTFTFramebuffer(Framebuffer).Lock = INVALID_HANDLE_VALUE then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end; 
    
    {Create Timer}
    PTFTFramebuffer(Framebuffer).Timer:=TimerCreateEx(MILLISECONDS_PER_SECOND div PTFTFramebuffer(Framebuffer).FrameRate,TIMER_STATE_DISABLED,TIMER_FLAG_WORKER,TTimerEvent(TFTFramebufferUpdateDisplay),Framebuffer); {Scheduled as required}
    if PTFTFramebuffer(Framebuffer).Timer = INVALID_HANDLE_VALUE then
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

function TFTFramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;
{Implementation of FramebufferDeviceRelease API for TFT Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceRelease instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF FRAMEBUFFER_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'TFT Framebuffer Release');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Destroy Timer}
    Result:=TimerDestroy(PTFTFramebuffer(Framebuffer).Timer);
    if Result <> ERROR_SUCCESS then Exit;
    PTFTFramebuffer(Framebuffer).Timer:=INVALID_HANDLE_VALUE;
   
    {Deinitialize Framebuffer}
    if Assigned(PTFTFramebuffer(Framebuffer).Deinitialize) then
     begin
      Result:=PTFTFramebuffer(Framebuffer).Deinitialize(PTFTFramebuffer(Framebuffer));
      if Result <> ERROR_SUCCESS then Exit;
     end; 
    
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
    MutexDestroy(PTFTFramebuffer(Framebuffer).Lock);
    PTFTFramebuffer(Framebuffer).Lock:=INVALID_HANDLE_VALUE;
    PTFTFramebuffer(Framebuffer).DirtyY1:=PTFTFramebuffer(Framebuffer).Height - 1;
    PTFTFramebuffer(Framebuffer).DirtyY2:=0;
    PTFTFramebuffer(Framebuffer).Ready:=True;
    
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

function TFTFramebufferMark(Framebuffer:PFramebufferDevice;X,Y,Width,Height,Flags:LongWord):LongWord;
{Implementation of FramebufferDeviceMark API for TFT Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceMark instead}
{Note: Marks full lines only, X and Width are ignored for TFT Framebuffer}
var
 {Clean:Boolean;}
 Enable:Boolean;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF FRAMEBUFFER_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'TFT Framebuffer Mark (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}
 
 {Acquire Dirty Region Lock}
 if MutexLock(PTFTFramebuffer(Framebuffer).Lock) = ERROR_SUCCESS then 
  begin
   try
    {Clean:=False;}
    Enable:=False;
    
    {Check Clean}
    {if PTFTFramebuffer(Framebuffer).DirtyY1 > PTFTFramebuffer(Framebuffer).DirtyY2 then
     begin
      Clean:=True;
     end;}
     
    {Check Y}
    if Y < PTFTFramebuffer(Framebuffer).DirtyY1 then
     begin
      PTFTFramebuffer(Framebuffer).DirtyY1:=Y;
      Enable:=True;
     end;
    
    {Check Height}
    if (Y + (Height - 1)) > PTFTFramebuffer(Framebuffer).DirtyY2 then
     begin
      PTFTFramebuffer(Framebuffer).DirtyY2:=(Y + (Height - 1));
      Enable:=True;
     end;
    
    {Check Enable and Ready}
    if Enable and (PTFTFramebuffer(Framebuffer).Ready) then
     begin
      {Enable Timer}
      Result:=TimerEnable(PTFTFramebuffer(Framebuffer).Timer);
      if Result = ERROR_SUCCESS then
       begin
        {Clear Ready}
        PTFTFramebuffer(Framebuffer).Ready:=False;
       end; 
     end
    else
     begin
      Result:=ERROR_SUCCESS;
     end;
   finally
    {Release Dirty Region Lock}
    MutexUnlock(PTFTFramebuffer(Framebuffer).Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TFTFramebufferCommit(Framebuffer:PFramebufferDevice;Address,Size,Flags:LongWord):LongWord;
{Implementation of FramebufferDeviceCommit API for TFT Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceCommit instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF FRAMEBUFFER_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'TFT Framebuffer Commit (Address=' + IntToHex(Address,8) + ' Size=' + IntToStr(Size) + ')');
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

function TFTFramebufferSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Implementation of FramebufferDeviceSetProperties API for TFT Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetProperties instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF FRAMEBUFFER_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'TFT Framebuffer Set Properties');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
 
    //To Do //Check Properties against current, modify if possible, otherwise reallocate ? (and Notify Resize)
    
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

procedure TFTFramebufferUpdateDisplay(Framebuffer:PTFTFramebuffer);
{Timer function for display dirty region redraw}
{Note: Not intended to be called directly by applications}
var
 Size:LongWord;
 Unlock:Boolean;
 Address:LongWord;
 DirtyY1:LongWord;
 DirtyY2:LongWord;
begin
 {}
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF FRAMEBUFFER_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'TFT Framebuffer Update Display');
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
      {$IFDEF FRAMEBUFFER_DEBUG}
      if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'TFT Framebuffer Error region start is greater than end (DirtyY1=' + IntToStr(DirtyY1) + ' DirtyY2=' + IntToStr(DirtyY2) + ')');
      {$ENDIF}
      {Set Ready}
      Framebuffer.Ready:=True;
      Exit;
      
      {Reset Region}
      {DirtyY1:=0;}
      {DirtyY2:=Framebuffer.Framebuffer.PhysicalHeight - 1;}
     end;
    
    {Check Dirty Region}
    if (DirtyY1 > (Framebuffer.Framebuffer.PhysicalHeight - 1)) or (DirtyY2 > (Framebuffer.Framebuffer.PhysicalHeight - 1)) then
     begin
      {$IFDEF FRAMEBUFFER_DEBUG}
      if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'TFT Framebuffer Error region start or end is greater than maximum (DirtyY1=' + IntToStr(DirtyY1) + ' DirtyY2=' + IntToStr(DirtyY2) + ')');
      {$ENDIF}
      {Set Ready}
      Framebuffer.Ready:=True;
      Exit;
     
      {Reset Region}
      {DirtyY1:=0;}
      {DirtyY2:=Framebuffer.Framebuffer.PhysicalHeight - 1;}
     end;
     
    if Assigned(Framebuffer.SetWriteAddress) then
     begin
      {Set Write Address}
      Framebuffer.SetWriteAddress(Framebuffer,0,DirtyY1,Framebuffer.Framebuffer.PhysicalWidth - 1,DirtyY2);
      
      {Get Size}
      Size:=((DirtyY2 - DirtyY1) + 1) * Framebuffer.Framebuffer.Pitch;
      
      {Get Address}
      Address:=Framebuffer.Framebuffer.Address + (DirtyY1 * Framebuffer.Framebuffer.Pitch);
      
      {Write Data}
      if Assigned(Framebuffer.WriteMemory) then
       begin
        {Release Dirty Region Lock}
        MutexUnlock(Framebuffer.Lock);
        Unlock:=False;
       
        Framebuffer.WriteMemory(Framebuffer,Address,Size);
        
        {Acquire Dirty Region Lock}
        if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
        Unlock:=True;
       end;
     end; 

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
{TFTFramebuffer Helper Functions}
 
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
 