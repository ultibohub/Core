{
Raspberry Pi FT5406 Touch Driver.

Copyright (C) 2019 - SoftOz Pty Ltd.

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

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

  Linux - \drivers\input\touchscreen\rpi-ft5406.c - Copyright (C) 2015 Raspberry Pi

References
==========
  
  FT5406 - http://www.haoyuelectronics.com/Attachment/HY101CTP/FocalTech-FT5x06%20DataSheet%20V4.0_1212.pdf
   
Raspberry Pi FT5406
===================

 This is the touchscreen driver for the Official Raspberry Pi 7" Touchscreen. While this device uses a
 FocalTech FT5406 10 point capacitive touchscreen controller it is actually connected to the GPU and not
 directly accessible to the ARM processor.
 
 In order to make the touchscreen data available the GPU provides a memory based interface that can be
 read by polling an address returned from a mailbox call.
 
 The Linux driver uses a thread to poll the data approximately 60 times per second so this driver does
 something similar.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit RPiFT5406;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,HeapManager,Threads,Devices,Touch,Mouse,SysUtils;
     
{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {RPiFT5406 specific constants}
 RPIFT5406_TOUCH_DESCRIPTION = 'Raspberry Pi FT5406 Touch Controller';  {Description of RPiFT5406 Touch device}
 
 RPIFT5406_THREAD_NAME = 'RPiFT5406 Touch'; {Name of the RPiFT5406 Touch polling thread}
 
 RPIFT5406_MAX_POINTS = 10;
 RPIFT5406_MAX_X = $FFF;
 RPIFT5406_MAX_Y = $FFF;
 RPIFT5406_MAX_Z = 0;
 
 RPIFT5406_SCREEN_WIDTH  = 800;
 RPIFT5406_SCREEN_HEIGHT = 480;
 
 RPIFT5406_TOUCH_DOWN    = 0;
 RPIFT5406_TOUCH_UP      = 1;
 RPIFT5406_TOUCH_CONTACT = 2;
 
{==============================================================================}
type
 {RPiFT5406 specific types}
 PRPiFT5406TouchPoint = ^TRPiFT5406TouchPoint;
 TRPiFT5406TouchPoint = record
  xh:Byte;
  xl:Byte;
  yh:Byte;
  yl:Byte;
  res1:Byte;
  res2:Byte;
 end;
 
 PRPiFT5406Registers = ^TRPiFT5406Registers;
 TRPiFT5406Registers = record
  DeviceMode:Byte;
  GestureId:Byte;
  NumPoints:Byte;
  Point:array[0..RPIFT5406_MAX_POINTS - 1] of TRPiFT5406TouchPoint;
 end;
 
 PRPiFT5406Touch = ^TRPiFT5406Touch;
 TRPiFT5406Touch = record
  {Touch Properties}
  Touch:TTouchDevice;
  {RPiFT5406 Properties}
  Thread:TThreadHandle;
  Terminate:Boolean;
  Registers:PRPiFT5406Registers;
 end;
 
{==============================================================================}
{var}
 {RPiFT5406 specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure RPiFT5406Init;

{==============================================================================}
{RPiFT5406 Functions}

{==============================================================================}
{RPiFT5406 Touch Functions}
function RPiFT5406TouchStart(Touch:PTouchDevice):LongWord;
function RPiFT5406TouchStop(Touch:PTouchDevice):LongWord;

function RPiFT5406TouchExecute(Touch:PRPiFT5406Touch):PtrInt;

{==============================================================================}
{RPiFT5406 Helper Functions}
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {RPiFT5406 specific variables}
 RPiFT5406Initialized:Boolean;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RPiFT5406Init;
{Initialize the RPiFT5406 unit and create, register and start the device}

{Note: Called only during system startup}
var
 Status:LongWord;

 RPiFT5406Touch:PRPiFT5406Touch;
begin
 {}
 {Check Initialized}
 if RPiFT5406Initialized then Exit;
 
 {Create Touch}
 RPiFT5406Touch:=PRPiFT5406Touch(TouchDeviceCreateEx(SizeOf(TRPiFT5406Touch)));
 if RPiFT5406Touch <> nil then
  begin
   {Update Touch}
   {Device}
   RPiFT5406Touch.Touch.Device.DeviceBus:=DEVICE_BUS_MMIO; 
   RPiFT5406Touch.Touch.Device.DeviceType:=TOUCH_TYPE_CAPACITIVE;
   RPiFT5406Touch.Touch.Device.DeviceFlags:=RPiFT5406Touch.Touch.Device.DeviceFlags or TOUCH_FLAG_MULTI_POINT;
   RPiFT5406Touch.Touch.Device.DeviceData:=nil;
   RPiFT5406Touch.Touch.Device.DeviceDescription:=RPIFT5406_TOUCH_DESCRIPTION;
   {Touch}
   RPiFT5406Touch.Touch.TouchState:=TOUCH_STATE_DISABLED;
   RPiFT5406Touch.Touch.DeviceStart:=RPiFT5406TouchStart;
   RPiFT5406Touch.Touch.DeviceStop:=RPiFT5406TouchStop;
   {Driver}
   RPiFT5406Touch.Touch.Properties.Flags:=RPiFT5406Touch.Touch.Device.DeviceFlags;
   RPiFT5406Touch.Touch.Properties.Width:=RPIFT5406_SCREEN_WIDTH;
   RPiFT5406Touch.Touch.Properties.Height:=RPIFT5406_SCREEN_HEIGHT;
   RPiFT5406Touch.Touch.Properties.Rotation:=TOUCH_ROTATION_0;
   RPiFT5406Touch.Touch.Properties.MaxX:=RPIFT5406_MAX_X;
   RPiFT5406Touch.Touch.Properties.MaxY:=RPIFT5406_MAX_Y;
   RPiFT5406Touch.Touch.Properties.MaxZ:=RPIFT5406_MAX_Z;
   RPiFT5406Touch.Touch.Properties.MaxPoints:=RPIFT5406_MAX_POINTS;
   {RPiFT5406}
   RPiFT5406Touch.Thread:=INVALID_HANDLE_VALUE;
   RPiFT5406Touch.Terminate:=False;
   RPiFT5406Touch.Registers:=nil;
   
   {Register Touch}
   Status:=TouchDeviceRegister(@RPiFT5406Touch.Touch);
   if Status = ERROR_SUCCESS then
    begin
     {Start Touch}
     Status:=TouchDeviceStart(@RPiFT5406Touch.Touch);
     if Status <> ERROR_SUCCESS then
      begin
       if TOUCH_LOG_ENABLED then TouchLogError(nil,'RPiFT5406: Failed to start new Touch device: ' + ErrorToString(Status));
       
       {Deregister Touch}
       TouchDeviceDeregister(@RPiFT5406Touch.Touch);
       
       {Destroy Touch}
       TouchDeviceDestroy(@RPiFT5406Touch.Touch);
      end;
    end
   else 
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(nil,'RPiFT5406: Failed to register new Touch device: ' + ErrorToString(Status));
     
     {Destroy Touch}
     TouchDeviceDestroy(@RPiFT5406Touch.Touch);
    end;
  end
 else 
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(nil,'RPiFT5406: Failed to create new Touch device');
  end; 
 
 RPiFT5406Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{RPiFT5406 Functions}

{==============================================================================}
{==============================================================================}
{RPiFT5406 Touch Functions}
function RPiFT5406TouchStart(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceStart API for RPiFT5406}
{Note: Not intended to be called directly by applications, use TouchDeviceStart instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Touch}
 if Touch = nil then Exit;
 
 {$IF DEFINED(RPIFT5406_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'RPiFT5406: Touch Start');
 {$ENDIF}

 {Create Thread} 
 PRPiFT5406Touch(Touch).Thread:=BeginThread(TThreadFunc(RPiFT5406TouchExecute),Touch,PRPiFT5406Touch(Touch).Thread,THREAD_STACK_DEFAULT_SIZE);
 if PRPiFT5406Touch(Touch).Thread = INVALID_HANDLE_VALUE then
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(nil,'RPiFT5406: Failed to create Touch device thread');
   Exit;
  end;
 
 {Set Thread Name}
 ThreadSetName(PRPiFT5406Touch(Touch).Thread,RPIFT5406_THREAD_NAME);
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function RPiFT5406TouchStop(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceStop API for RPiFT5406}
{Note: Not intended to be called directly by applications, use TouchDeviceStop instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Touch}
 if Touch = nil then Exit;
 
 {$IF DEFINED(RPIFT5406_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'RPiFT5406: Touch Stop');
 {$ENDIF}
 
 if PRPiFT5406Touch(Touch).Thread <> INVALID_HANDLE_VALUE then
  begin
   {Signal Terminate}
   PRPiFT5406Touch(Touch).Terminate:=True;
   
   {Wait}
   ThreadWaitTerminate(PRPiFT5406Touch(Touch).Thread,INFINITE);
   PRPiFT5406Touch(Touch).Thread:=INVALID_HANDLE_VALUE;
  end;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function RPiFT5406TouchExecute(Touch:PRPiFT5406Touch):PtrInt;
{Thread function for the RPiFT5406 Touch controller driver. The thread polls the
 memory touch buffer approximately 60 times per second for new touch data and
 inserts received touch points into the buffer of the passed device}
{Note: Not intended to be called directly by applications} 
var
 X:Word;
 Y:Word;
 TouchID:Word;
 EventType:Word;
 Size:LongWord;
 Count:LongWord;
 Buffer:Pointer;
 Status:LongWord;
 Address:LongWord;
 BoardType:LongWord;
 CachedBuffer:Boolean;
 TouchData:PTouchData;
 MouseData:TMouseData;
 LastPoints:LongWord;
 ModifiedPoints:LongWord;
 ReleasedPoints:LongWord;
 Registers:TRPiFT5406Registers;
begin
 {}
 Result:=0;
 
 {Check Touch}
 if Touch = nil then Exit;
 
 {Setup Defaults}
 Buffer:=nil;
 CachedBuffer:=False;
 
 {Get Size}
 Size:=RoundUp(MEMORY_PAGE_SIZE,DMA_MULTIPLIER);

 {Get Board Type}
 BoardType:=BoardGetType;
 
 {Check Registers}
 while Touch.Registers = nil do
  begin
   {Check Buffer}
   if Buffer = nil then 
    begin
     {Check Board Type}
     case BoardType of
      BOARD_TYPE_RPIA,
      BOARD_TYPE_RPIB,
      BOARD_TYPE_RPIA_PLUS,
      BOARD_TYPE_RPIB_PLUS,
      BOARD_TYPE_RPI_COMPUTE,
      BOARD_TYPE_RPI_ZERO,
      BOARD_TYPE_RPI_ZERO_W:begin
        {Allocate Shared}
        Buffer:=AllocSharedAlignedMem(Size,DMA_ALIGNMENT);
       end;
      BOARD_TYPE_RPI2B,
      BOARD_TYPE_RPI3B,
      BOARD_TYPE_RPI3B_PLUS,
      BOARD_TYPE_RPI3A_PLUS,
      BOARD_TYPE_RPI_COMPUTE3,
      BOARD_TYPE_RPI_COMPUTE3_PLUS:begin
        {Allocate Non Cached}
        Buffer:=AllocNoCacheAlignedMem(Size,DMA_ALIGNMENT);
       end;
     end;
     
     {Check Buffer}
     if Buffer = nil then 
      begin
       {Allocate Normal}
       Buffer:=AllocAlignedMem(Size,DMA_ALIGNMENT);
       
       {Set Caching}
       CachedBuffer:=not(DMA_CACHE_COHERENT);
      end;
    end;  
   
   {Check Buffer}
   if Buffer <> nil then
    begin
     {Set Touch Buffer}
     Address:=PhysicalToBusAddress(Buffer);
     Status:=TouchSetBuffer(Address);
     if Status = ERROR_SUCCESS then
      begin
       {Get Registers}
       Touch.Registers:=PRPiFT5406Registers(Buffer);
       
       Break;
      end
     else
      begin
       {$IF DEFINED(RPIFT5406_DEBUG) or DEFINED(TOUCH_DEBUG)}
       if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'RPiFT5406: TouchSetBuffer Failed (Status=' + IntToHex(Status,8) + ')');
       {$ENDIF}
       
       {Get Touch Buffer}
       Address:=0;
       Status:=TouchGetBuffer(Address);
       if Status = ERROR_SUCCESS then
        begin
         if Address <> 0 then
          begin
           {Get Registers}
           Touch.Registers:=PRPiFT5406Registers(BusAddressToPhysical(Pointer(Address)));
           
           {Set Caching}
           CachedBuffer:=True;
           
           {Free Buffer}
           FreeMem(Buffer);
           Buffer:=nil;
           
           Break;
          end;
        end
       else
        begin
         {$IF DEFINED(RPIFT5406_DEBUG) or DEFINED(TOUCH_DEBUG)}
         if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'RPiFT5406: TouchGetBuffer Failed (Status=' + IntToHex(Status,8) + ')');
         {$ENDIF}
        end; 
      end;
    end; 
   
   {Check Terminate}
   if Touch.Terminate then Break;
   
   ThreadSleep(1000);
  end;

 {$IF DEFINED(RPIFT5406_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'RPiFT5406: Touch Execute (Registers=' + IntToHex(LongWord(Touch.Registers),8) + ' Caching=' + BoolToStr(CachedBuffer) + ')');
 {$ENDIF}
 
 {Setup Defaults}
 LastPoints:=0;
 
 {Poll Registers}
 while not(Touch.Terminate) do
  begin
   ThreadSleep(17); {17ms equals approx 60 times per second}
   
   {Invalidate Cache}
   if CachedBuffer then InvalidateDataCacheRange(LongWord(Touch.Registers),SizeOf(TRPiFT5406Registers));
   
   {Copy Registers}
   System.Move(Touch.Registers^,Registers,SizeOf(TRPiFT5406Registers));
   
   {Write 99 to the NumPoints registers so we know if the GPU modified them}
   Touch.Registers.NumPoints:=99;
   
   {Clean Cache}
   if CachedBuffer then CleanDataCacheRange(LongWord(Touch.Registers),SizeOf(TRPiFT5406Registers));
   
   {Check if anything changed}
   if (Registers.NumPoints <> 99) and ((Registers.NumPoints <> 0) or (LastPoints <> 0)) then
    begin
     {Acquire Lock}
     if MutexLock(Touch.Touch.Lock) = ERROR_SUCCESS then
      begin
       {$IF DEFINED(RPIFT5406_DEBUG) or DEFINED(TOUCH_DEBUG)}
       if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'RPiFT5406: Touch Execute (NumPoints=' + IntToStr(Registers.NumPoints) + ')');
       {$ENDIF}
       
       {Setup Defaults}
       ModifiedPoints:=0;
       
       {Check Modified Points}
       if Registers.NumPoints > 0 then
        begin
         for Count:=0 to Registers.NumPoints - 1 do
          begin
           {Get X and Y}
           X:=((Registers.Point[Count].xh and $F) shl 8) or Registers.Point[Count].xl;
           Y:=((Registers.Point[Count].yh and $F) shl 8) or Registers.Point[Count].yl;
           TouchID:=(Registers.Point[Count].yh shr 4) and $F;
           EventType:=(Registers.Point[Count].yh shr 6) and $03;
           
           {$IF DEFINED(RPIFT5406_DEBUG) or DEFINED(TOUCH_DEBUG)}
           if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'RPiFT5406:  Modified X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' TouchID=' + IntToStr(TouchID) + ' EventType=' + IntToStr(EventType));
           {$ENDIF}
           
           {Store Point}
           ModifiedPoints:=ModifiedPoints or (1 shl TouchID);
          
           {Check Event Type}
           if (EventType = RPIFT5406_TOUCH_DOWN) or (EventType = RPIFT5406_TOUCH_CONTACT) then
            begin
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
                   {Update Touch Data}
                   TouchData.Info:=TOUCH_FINGER;
                   TouchData.PointID:=TouchID + 1;
                   {Check Rotation}
                   case Touch.Touch.Properties.Rotation of
                    TOUCH_ROTATION_0:begin
                      {No Change}
                      TouchData.PositionX:=X;
                      TouchData.PositionY:=Y;
                     end;
                    TOUCH_ROTATION_90:begin
                      {Swap X and Y}
                      TouchData.PositionX:=Y;
                      TouchData.PositionY:=X;
                     end;
                    TOUCH_ROTATION_180:begin
                      {Invert X and Y}
                      TouchData.PositionX:=Touch.Touch.Properties.MaxX - X;
                      TouchData.PositionY:=Touch.Touch.Properties.MaxY - Y;
                     end;
                    TOUCH_ROTATION_270:begin
                      {Swap and Invert X and Y}
                      TouchData.PositionX:=Touch.Touch.Properties.MaxX - Y;
                      TouchData.PositionY:=Touch.Touch.Properties.MaxY - X;
                     end;
                   end;
                   TouchData.PositionZ:=0;
                   //To Do //Continuing //Calibration
                   
                   {Update Count}
                   Inc(Touch.Touch.Buffer.Count);
                   
                   {Signal Data Received}
                   SemaphoreSignal(Touch.Touch.Buffer.Wait);
                  end;
                end
               else
                begin
                 if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'RPiFT5406: Buffer overflow, packet discarded'); 
                   
                 {Update Statistics}
                 Inc(Touch.Touch.BufferOverruns); 
                end;                           
              end
             else
              begin
               {For mouse report the first point}
               if TouchID = 0 then
                begin
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
                    {Swap X and Y}
                    MouseData.OffsetX:=Y;
                    MouseData.OffsetY:=X;
                   end;
                  TOUCH_ROTATION_180:begin
                    {Invert X and Y}
                    MouseData.OffsetX:=Touch.Touch.Properties.MaxX - X;
                    MouseData.OffsetY:=Touch.Touch.Properties.MaxY - Y;
                   end;
                  TOUCH_ROTATION_270:begin
                    {Swap and Invert X and Y}
                    MouseData.OffsetX:=Touch.Touch.Properties.MaxX - Y;
                    MouseData.OffsetY:=Touch.Touch.Properties.MaxY - X;
                   end;
                 end;
                 MouseData.OffsetWheel:=0;
                 //To Do //Continuing //Calibration
                 
                 {Maximum X, Y and Wheel}
                 MouseData.MaximumX:=Touch.Touch.Properties.MaxX; {Touch.Touch.Properties.Width}
                 MouseData.MaximumY:=Touch.Touch.Properties.MaxY; {Touch.Touch.Properties.Height}
                 MouseData.MaximumWheel:=0;
                 
                 {Write Mouse Data}
                 if MouseWrite(@MouseData,SizeOf(TMouseData),1) <> ERROR_SUCCESS then
                  begin
                   if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'RPiFT5406: Failed to write mouse data, packet discarded'); 
                   
                   {Update Statistics}
                   Inc(Touch.Touch.ReceiveErrors); 
                  end;
                end;
              end; 
            end;
          end;
        end;
        
       {$IF DEFINED(RPIFT5406_DEBUG) or DEFINED(TOUCH_DEBUG)}
       if TOUCH_LOG_ENABLED and (ModifiedPoints > 0) then TouchLogDebug(@Touch.Touch,'RPiFT5406:  Modified Points=' + IntToHex(ModifiedPoints,8));
       {$ENDIF}
        
       {Check Released Points}
       ReleasedPoints:=LastPoints and not(ModifiedPoints);
       if ReleasedPoints > 0 then
        begin
         for Count:=0 to RPIFT5406_MAX_POINTS - 1 do
          begin
           if (ReleasedPoints and (1 shl Count)) <> 0 then
            begin
             {$IF DEFINED(RPIFT5406_DEBUG) or DEFINED(TOUCH_DEBUG)}
             if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'RPiFT5406:  Released TouchID=' + IntToStr(Count));
             {$ENDIF}
  
             //To Do //Do we need to update ModifiedPoints ? //See Linux driver //No ?
             
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
                   {Update Touch Data}
                   TouchData.Info:=0;
                   TouchData.PointID:=Count + 1;
                   TouchData.PositionX:=TOUCH_X_UNKNOWN;
                   TouchData.PositionY:=TOUCH_Y_UNKNOWN;
                   TouchData.PositionZ:=TOUCH_Z_UNKNOWN;
               
                   {Update Count}
                   Inc(Touch.Touch.Buffer.Count);
                   
                   {Signal Data Received}
                   SemaphoreSignal(Touch.Touch.Buffer.Wait);
                  end;
                end
               else
                begin
                 if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'RPiFT5406: Buffer overflow, packet discarded'); 
                   
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
                 MouseData.MaximumX:=Touch.Touch.Properties.MaxX; {Touch.Touch.Properties.Width}
                 MouseData.MaximumY:=Touch.Touch.Properties.MaxY; {Touch.Touch.Properties.Height}
                 MouseData.MaximumWheel:=0;
                 
                 {Write Mouse Data}
                 if MouseWrite(@MouseData,SizeOf(TMouseData),1) <> ERROR_SUCCESS then
                  begin
                   if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'RPiFT5406: Failed to write mouse data, packet discarded'); 
                   
                   {Update Statistics}
                   Inc(Touch.Touch.ReceiveErrors); 
                  end;
                end;
              end; 
            end; 
          end; 
        end; 
  
       {Save Last Points}
       LastPoints:=ModifiedPoints;
     
       {Release Lock}
       MutexUnlock(Touch.Touch.Lock);
      end;      
    end;
  end; 
  
 {Note: Do not free buffer, firmware may still be writing data to it} 
end;

{==============================================================================}
{==============================================================================}
{RPiFT5406 Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 RPiFT5406Init;
 
{==============================================================================}
 
{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
