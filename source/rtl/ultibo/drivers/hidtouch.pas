{
Ultibo HID Touch consumer unit.

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


HID Touch
=========

 This is a consumer for any generic HID touch screen, it accepts HID application
 collections in the digitizer device page (HID_PAGE_DIGITIZERS) with the usage
 set to touch screen (HID_DIGITIZERS_TOUCH_SCREEN).

 The consumer will bind to any touch collection that implements at a minimum the
 X and Y axis and the tip switch. The event data for in range, confidence, width
 and height are not currently reported but may be added if required.

 To prevent the HID mouse consumer from binding to touch screen devices that also
 include a mouse collection, this consumer sets the HID_MOUSE_REJECT_TOUCH variable
 to True during initialization.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit HIDTouch;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,HID,Touch,Mouse,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {HID Touch specific constants}
 HID_TOUCH_CONSUMER_NAME = 'HID Touch Consumer';     {Name of HID Touch consumer}

 HID_TOUCH_DESCRIPTION = 'HID Touch'; {Description of HID Touch device}

{==============================================================================}
type
 {HID Touch specific types}
 PHIDTouchPoint = ^THIDTouchPoint;
 THIDTouchPoint = record
  X:LongWord;
  Y:LongWord;
  Width:LongWord;
  Height:LongWord;
  Pressure:LongWord;
  TipSwitch:Boolean;
  InRange:Boolean;
  Confidence:Boolean;
  Identifier:LongWord;
 end;

 PHIDTouchPoints = ^THIDTouchPoints;
 THIDTouchPoints = array[0..0] of THIDTouchPoint;

 PHIDTouchDevice = ^THIDTouchDevice;
 THIDTouchDevice = record
  {Touch Properties}
  Touch:TTouchDevice;
  {General Properties}
  MaxX:LongWord;                    {Maximum X value from current configuration}
  MaxY:LongWord;                    {Maximum Y value from current configuration}
  MaxZ:LongWord;                    {Maximum Z value from current configuration}
  MaxWidth:LongWord;                {Maximum width value from current configuration}
  MaxHeight:LongWord;               {Maximum height value from current configuration}
  MaxPoints:LongWord;               {Maximum touch points for this device}
  LastCount:LongWord;               {Number of touch points for last touch report}
  LastPoints:PHIDTouchPoints;       {Touch points for last touch report}
  TouchPoints:PHIDTouchPoints;      {Touch points for current touch report}
  FirstIdentifier:LongWord;         {Id number of first touch point identifier}
  {HID Properties}
  Timer:TTimerHandle;               {Handle for touch release timer}
  Collection:PHIDCollection;        {The HID collection this touch is bound to}
  Definitions:PHIDDefinition;       {The input report definitions that can be accepted as touch reports}
  FeatureDefinition:PHIDDefinition; {The report definition that corresponds to the maximum contact count feature}
 end;

{==============================================================================}
{var}
 {HID Touch specific variables}

{==============================================================================}
{Initialization Functions}
procedure HIDTouchInit;

{==============================================================================}
{HID Touch Functions}
function HIDTouchStart(Touch:PTouchDevice):LongWord;
function HIDTouchStop(Touch:PTouchDevice):LongWord;

function HIDTouchUpdate(Touch:PTouchDevice):LongWord;

{==============================================================================}
{HID Touch Helper Functions}
function HIDTouchCheckCollection(Collection:PHIDCollection):LongWord;
function HIDTouchCheckInputDefinition(Definition:PHIDDefinition):LongWord;
function HIDTouchCheckFeatureDefinition(Definition:PHIDDefinition):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {HID Touch specific variables}
 HIDTouchInitialized:Boolean;

 HIDTouchConsumer:PHIDConsumer; {HID Consumer interface (Set by HIDTouchInit)}

{==============================================================================}
{==============================================================================}
{Forward Declarations}
function HIDTouchCollectionBind(Device:PHIDDevice;Collection:PHIDCollection):LongWord; forward;
function HIDTouchCollectionUnbind(Device:PHIDDevice;Collection:PHIDCollection):LongWord; forward;

function HIDTouchReportReceive(Collection:PHIDCollection;ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord; forward;

procedure HIDTouchTimer(Touch:PHIDTouchDevice); forward;

function HIDTouchGetMaxXYZ(Touch:PHIDTouchDevice;var MaxX,MaxY,MaxZ,MaxWidth,MaxHeight:LongWord):LongWord; forward;
function HIDTouchGetMaxPoints(Touch:PHIDTouchDevice;var MaxPoints:LongWord):LongWord; forward;

function HIDTouchExtractReport(Touch:PHIDTouchDevice;Definition:PHIDDefinition;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure HIDTouchInit;
{Initialize the HID Touch unit and HID Touch driver}

{Note: Called only during system startup}
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if HIDTouchInitialized then Exit;

 if HID_REGISTER_TOUCH then
  begin
   {Update Mouse Configuration}
   HID_MOUSE_REJECT_TOUCH:=True;

   {Create HID Touch Consumer}
   HIDTouchConsumer:=HIDConsumerCreate;
   if HIDTouchConsumer <> nil then
    begin
     {Update HID Touch Consumer}
     {Driver}
     HIDTouchConsumer.Driver.DriverName:=HID_TOUCH_CONSUMER_NAME;
     {HID}
     HIDTouchConsumer.CollectionBind:=HIDTouchCollectionBind;
     HIDTouchConsumer.CollectionUnbind:=HIDTouchCollectionUnbind;
     HIDTouchConsumer.ReportReceive:=HIDTouchReportReceive;

     {Register HID Touch Consumer}
     Status:=HIDConsumerRegister(HIDTouchConsumer);
     if Status <> ERROR_SUCCESS then
      begin
       if HID_LOG_ENABLED then HIDLogError(nil,'Touch: Failed to register HID Touch consumer: ' + ErrorToString(Status));

       {Destroy Consumer}
       HIDConsumerDestroy(HIDTouchConsumer);

       HIDTouchConsumer:=nil;
      end;
    end
   else
    begin
     if HID_LOG_ENABLED then HIDLogError(nil,'Touch: Failed to create HID Touch consumer');
    end;
  end;

 HIDTouchInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{HID Touch Functions}
function HIDTouchStart(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceStart API for HID Touch device}
{Note: Not intended to be called directly by applications, use TouchDeviceStart instead}
var
 Status:LongWord;
 Device:PHIDDevice;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'HID: Touch Start');
 {$ENDIF}

 {Get Device}
 Device:=PHIDDevice(Touch.Device.DeviceData);
 if Device = nil then Exit;

 {Update Configuration}
 Status:=HIDTouchUpdate(Touch);
 if Status <> ERROR_SUCCESS then
  begin
   if HID_LOG_ENABLED then HIDLogError(Device,'Touch: Failed to update configuration: ' + ErrorToString(Status));

   Result:=Status;
   Exit;
  end;

 {Submit Reports}
 Definition:=PHIDTouchDevice(Touch).Definitions;
 while Definition <> nil do
  begin
   Status:=HIDDeviceSubmitReport(Device,Definition.Id);
   if Status <> ERROR_SUCCESS then
    begin
     if HID_LOG_ENABLED then HIDLogError(Device,'Touch: Failed to submit report id ' + IntToStr(Definition.Id) + ': ' + ErrorToString(Status));

     Result:=Status;
     Exit;
    end;

   {Get Next Definition}
   Definition:=Definition.Next;
  end;

 {Create Timer}
 PHIDTouchDevice(Touch).Timer:=TimerCreateEx(100,TIMER_STATE_DISABLED,TIMER_FLAG_WORKER,TTimerEvent(HIDTouchTimer),Touch); {Scheduled by Report Worker}

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDTouchStop(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceStop API for HID Touch device}
{Note: Not intended to be called directly by applications, use TouchDeviceStop instead}
var
 Device:PHIDDevice;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'HID: Touch Stop');
 {$ENDIF}

 {Get Device}
 Device:=PHIDDevice(Touch.Device.DeviceData);
 if Device = nil then Exit;

 {Cancel Timer}
 TimerDestroy(PHIDTouchDevice(Touch).Timer);
 PHIDTouchDevice(Touch).Timer:=INVALID_HANDLE_VALUE;

 {Cancel Reports}
 Definition:=PHIDTouchDevice(Touch).Definitions;
 while Definition <> nil do
  begin
   {Cancel Report}
   HIDDeviceCancelReport(Device,Definition.Id);

   {Get Next Definition}
   Definition:=Definition.Next;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDTouchUpdate(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceUpdate API for HID Touch device}
{Note: Not intended to be called directly by applications, use TouchDeviceUpdate instead}
var
 MaxX:LongWord;
 MaxY:LongWord;
 MaxZ:LongWord;
 MaxWidth:LongWord;
 MaxHeight:LongWord;
 MaxPoints:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'HID: Touch Update');
 {$ENDIF}

 {Acquire Lock}
 if MutexLock(Touch.Lock) = ERROR_SUCCESS then
  begin
   try
    {Set Defaults}
    MaxX:=0;
    MaxY:=0;
    MaxZ:=0;
    MaxWidth:=0;
    MaxHeight:=0;
    MaxPoints:=0;

    {Get Max Points}
    Result:=HIDTouchGetMaxPoints(PHIDTouchDevice(Touch),MaxPoints);
    if Result <> ERROR_SUCCESS then Exit;

    {Get Max X, Y, Z}
    Result:=HIDTouchGetMaxXYZ(PHIDTouchDevice(Touch),MaxX,MaxY,MaxZ,MaxWidth,MaxHeight);
    if Result <> ERROR_SUCCESS then Exit;

    {Update Max X, Y, Z, Width, Height}
    PHIDTouchDevice(Touch).MaxX:=MaxX;
    PHIDTouchDevice(Touch).MaxY:=MaxY;
    PHIDTouchDevice(Touch).MaxZ:=MaxZ;
    PHIDTouchDevice(Touch).MaxWidth:=MaxWidth;
    PHIDTouchDevice(Touch).MaxHeight:=MaxHeight;

    {Check Max Points}
    if MaxPoints <> PHIDTouchDevice(Touch).MaxPoints then
     begin
      {Update Max Points}
      PHIDTouchDevice(Touch).MaxPoints:=MaxPoints;

      {Allocate Touch Points}
      if PHIDTouchDevice(Touch).TouchPoints <> nil then
       begin
        FreeMem(PHIDTouchDevice(Touch).TouchPoints);
       end;
      PHIDTouchDevice(Touch).TouchPoints:=AllocMem(MaxPoints * SizeOf(THIDTouchPoint));

      {Allocate Last Points}
      if PHIDTouchDevice(Touch).LastPoints <> nil then
       begin
        FreeMem(PHIDTouchDevice(Touch).LastPoints);
       end;
      PHIDTouchDevice(Touch).LastCount:=0;
      PHIDTouchDevice(Touch).LastPoints:=AllocMem(MaxPoints * SizeOf(THIDTouchPoint));

      {Reset First Identifier}
      PHIDTouchDevice(Touch).FirstIdentifier:=INFINITE;
     end;

    {Check Rotation}
    case Touch.Properties.Rotation of
     TOUCH_ROTATION_0,TOUCH_ROTATION_180:begin
       {Update Width and Height}
       Touch.Properties.Width:=PHIDTouchDevice(Touch).MaxX;
       Touch.Properties.Height:=PHIDTouchDevice(Touch).MaxY;

       {Update Max X and Y}
       if (Touch.Device.DeviceFlags and TOUCH_FLAG_SWAP_MAX_XY) = 0 then
        begin
         Touch.Properties.MaxX:=PHIDTouchDevice(Touch).MaxX;
         Touch.Properties.MaxY:=PHIDTouchDevice(Touch).MaxY;
        end
       else
        begin
         Touch.Properties.MaxX:=PHIDTouchDevice(Touch).MaxY;
         Touch.Properties.MaxY:=PHIDTouchDevice(Touch).MaxX;
        end;

       {Update Max Width and Height}
       Touch.Properties.MaxWidth:=PHIDTouchDevice(Touch).MaxWidth;
       Touch.Properties.MaxHeight:=PHIDTouchDevice(Touch).MaxHeight;
      end;
     TOUCH_ROTATION_90,TOUCH_ROTATION_270:begin
       {Update Width and Height}
       Touch.Properties.Width:=PHIDTouchDevice(Touch).MaxY;
       Touch.Properties.Height:=PHIDTouchDevice(Touch).MaxX;

       {Update Max X and Y}
       if (Touch.Device.DeviceFlags and TOUCH_FLAG_SWAP_MAX_XY) = 0 then
        begin
         Touch.Properties.MaxX:=PHIDTouchDevice(Touch).MaxY;
         Touch.Properties.MaxY:=PHIDTouchDevice(Touch).MaxX;
        end
       else
        begin
         Touch.Properties.MaxX:=PHIDTouchDevice(Touch).MaxX;
         Touch.Properties.MaxY:=PHIDTouchDevice(Touch).MaxY;
        end;

       {Update Max Width and Height}
       Touch.Properties.MaxWidth:=PHIDTouchDevice(Touch).MaxHeight;
       Touch.Properties.MaxHeight:=PHIDTouchDevice(Touch).MaxWidth;
      end;
    end;

    {Update Max Points}
    Touch.Properties.MaxPoints:=PHIDTouchDevice(Touch).MaxPoints;

    {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
    if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'HID:  Max Points: ' + IntToStr(Touch.Properties.MaxPoints) + ' Max X: ' + IntToStr(Touch.Properties.MaxX) + ' Max Y: ' + IntToStr(Touch.Properties.MaxY) + ' Max Z: ' + IntToStr(Touch.Properties.MaxZ));
    {$ENDIF}

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

{==============================================================================}
{==============================================================================}
{HID Touch Helper Functions}
function HIDTouchCheckCollection(Collection:PHIDCollection):LongWord;
{Check if a HID collection is suitable for use as a touch device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Collection}
 if Collection = nil then Exit;

 {Set Result}
 Result:=ERROR_NOT_SUPPORTED;

 {Check Page}
 case Collection.Page of
  HID_PAGE_DIGITIZERS:begin
    {Check Usage}
    case Collection.Usage of
     HID_DIGITIZERS_TOUCH_SCREEN:begin
       {Check Flags}
       if Collection.Flags = HID_MAIN_COLLECTION_APPLICATION then Result:=ERROR_SUCCESS;
      end;
    end;
   end;
 end;
end;

{==============================================================================}

function HIDTouchCheckInputDefinition(Definition:PHIDDefinition):LongWord;
{Check if a HID definition is suitable for use as a touch input report}
var
 HasX:Boolean;
 HasY:Boolean;
 HasTip:Boolean;
 Field:PHIDField;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Definition}
 if Definition = nil then Exit;

 {Set Defaults}
 HasX:=False;
 HasY:=False;
 HasTip:=False;

 {Set Result}
 Result:=ERROR_NOT_SUPPORTED;

 {Check Kind}
 if Definition.Kind <> HID_REPORT_INPUT then Exit;

 {Check Fields}
 Field:=Definition.Fields;
 while Field <> nil do
  begin
   {Check Page}
   case Field.Page of
    HID_PAGE_GENERIC_DESKTOP:begin
      {Check X}
      if (Field.Usage >= HID_DESKTOP_X) and (Field.Usage + Field.Count - 1 <= HID_DESKTOP_X) then
       begin
        HasX:=True;
       end;

      {Check Y}
      if (Field.Usage >= HID_DESKTOP_Y) and (Field.Usage + Field.Count - 1 <= HID_DESKTOP_Y) then
       begin
        HasY:=True;
       end;
     end;
    HID_PAGE_DIGITIZERS:begin
      {Check Tip Switch}
      if (Field.Usage >= HID_DIGITIZERS_TIP_SWITCH) and (Field.Usage + Field.Count - 1 <= HID_DIGITIZERS_TIP_SWITCH) then
       begin
        HasTip:=True;
       end;
     end;
   end;

   {Get Next Field}
   Field:=Field.Next;
  end;

 {Check Result}
 if HasX and HasY and HasTip then Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDTouchCheckFeatureDefinition(Definition:PHIDDefinition):LongWord;
{Check if a HID definition is suitable for use as a touch feature report}

{Note: A touch feature report is commonly used to report the maximum number of contacts}
var
 Field:PHIDField;
 HasMaximum:Boolean;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Definition}
 if Definition = nil then Exit;

 {Set Defaults}
 HasMaximum:=False;

 {Set Result}
 Result:=ERROR_NOT_SUPPORTED;

 {Check Kind}
 if Definition.Kind <> HID_REPORT_FEATURE then Exit;

 {Check Fields}
 Field:=Definition.Fields;
 while Field <> nil do
  begin
   {Check Page}
   case Field.Page of
    HID_PAGE_DIGITIZERS:begin
      {Check Maximum}
      if (Field.Usage >= HID_DIGITIZERS_CONTACT_COUNT_MAXIMUM) and (Field.Usage + Field.Count - 1 <= HID_DIGITIZERS_CONTACT_COUNT_MAXIMUM) then
       begin
        HasMaximum:=True;
       end;
     end;
   end;

   {Get Next Field}
   Field:=Field.Next;
  end;

 {Check Result}
 if HasMaximum then Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{HID Touch Internal Functions}
function HIDTouchCollectionBind(Device:PHIDDevice;Collection:PHIDCollection):LongWord;
{Implementation of HIDCollectionBind API for HID Touch}
{Note: Not intended to be called directly by applications}
var
 MinId:Byte;
 MaxId:Byte;
 Index:LongWord;
 Status:LongWord;
 Touch:PHIDTouchDevice;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Collection}
 if Collection = nil then Exit;

 {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Device,'Touch: Attempting to bind HID collection (Name=' + DeviceGetName(@Device.Device) + ' Description=' + Device.Device.DeviceDescription + ' Usage=' + IntToHex(Collection.Page,4) + IntToHex(Collection.Usage,4) + ')');
 {$ENDIF}

 {Set Result}
 Result:=ERROR_NOT_SUPPORTED;

 {Check Collection}
 Status:=HIDTouchCheckCollection(Collection);
 if Status <> ERROR_SUCCESS then Exit;

 {Create Touch}
 Touch:=PHIDTouchDevice(TouchDeviceCreateEx(SizeOf(THIDTouchDevice)));
 if Touch = nil then
  begin
   if HID_LOG_ENABLED then HIDLogError(Device,'Touch: Failed to create new touch device');

   Exit;
  end;

 {Update Touch}
 {Device}
 Touch.Touch.Device.DeviceBus:=DEVICE_BUS_NONE;
 Touch.Touch.Device.DeviceType:=TOUCH_TYPE_NONE;
 if Device.Device.DeviceType = HID_TYPE_USB then
  begin
   Touch.Touch.Device.DeviceBus:=DEVICE_BUS_USB;
  end;
 Touch.Touch.Device.DeviceFlags:=Touch.Touch.Device.DeviceFlags; {Don't override defaults (was TOUCH_FLAG_NONE)}
 Touch.Touch.Device.DeviceData:=Device;
 Touch.Touch.Device.DeviceDescription:=Device.Device.DeviceDescription; {HID_TOUCH_DESCRIPTION}
 {Touch}
 Touch.Touch.TouchState:=TOUCH_STATE_DISABLED;
 Touch.Touch.DeviceStart:=HIDTouchStart;
 Touch.Touch.DeviceStop:=HIDTouchStop;
 Touch.Touch.DeviceUpdate:=HIDTouchUpdate;
 {Driver}
 Touch.Touch.Properties.Flags:=Touch.Touch.Device.DeviceFlags;
 Touch.Touch.Properties.Width:=0;
 Touch.Touch.Properties.Height:=0;
 Touch.Touch.Properties.Rotation:=TOUCH_ROTATION_0;
 Touch.Touch.Properties.MaxX:=0;
 Touch.Touch.Properties.MaxY:=0;
 Touch.Touch.Properties.MaxZ:=0;
 Touch.Touch.Properties.MaxWidth:=0;
 Touch.Touch.Properties.MaxHeight:=0;
 Touch.Touch.Properties.MaxPoints:=0;
 {General}
 Touch.MaxPoints:=1;
 Touch.FirstIdentifier:=INFINITE;
 {HID}
 Touch.Timer:=INVALID_HANDLE_VALUE;
 Touch.Collection:=Collection;

 try
  {Get Report Ids}
  Status:=HIDFindReportIds(Device,Collection,MinId,MaxId);
  if Status <> ERROR_SUCCESS then Exit;

  {Allocate Definitions}
  for Index:=MinId to MaxId do
   begin
    {Input}
    Definition:=HIDAllocateDefinition(Device,Collection,HID_REPORT_INPUT,Index);
    if Definition <> nil then
     begin
      {Check Definition}
      if HIDTouchCheckInputDefinition(Definition) = ERROR_SUCCESS then
       begin
        {Link Definition}
        Definition.Next:=Touch.Definitions;
        Touch.Definitions:=Definition;
       end
      else
       begin
        {Free Definition}
        HIDFreeDefinition(Definition);
       end;
     end;

    {Feature}
    Definition:=HIDAllocateDefinition(Device,Collection,HID_REPORT_FEATURE,Index);
    if Definition <> nil then
     begin
      if HIDTouchCheckFeatureDefinition(Definition) = ERROR_SUCCESS then
       begin
        {Set Feature Definition}
        Touch.FeatureDefinition:=Definition;
       end
      else
       begin
        {Free Definition}
        HIDFreeDefinition(Definition);
       end;
     end;
   end;

  {Check Definitions}
  if Touch.Definitions = nil then
   begin
    if HID_LOG_ENABLED then HIDLogError(Device,'Touch: Failed to allocate definitions');

    Exit;
   end;

  {Allocate Reports}
  Definition:=Touch.Definitions;
  while Definition <> nil do
   begin
    {Allocate Report}
    Status:=HIDDeviceAllocateReport(Device,Collection,Definition.Id,Definition.Size);
    if Status <> ERROR_SUCCESS then
     begin
      if HID_LOG_ENABLED then HIDLogError(Device,'Touch: Failed to allocate report id ' + IntToStr(Definition.Id) + ': ' + ErrorToString(Status));

      Exit;
     end;

    {Get Next Definition}
    Definition:=Definition.Next;
   end;

  {Register Touch}
  Status:=TouchDeviceRegister(@Touch.Touch);
  if Status <> ERROR_SUCCESS then
   begin
    if HID_LOG_ENABLED then HIDLogError(Device,'Touch: Failed to register new touch device: ' + ErrorToString(Status));

    Exit;
   end;

  {Check Interval}
  if USB_TOUCH_POLLING_INTERVAL > 0 then
   begin
    {Set Interval}
    HIDDeviceSetInterval(Device,USB_TOUCH_POLLING_INTERVAL);
   end;

  {Update Collection}
  Collection.PrivateData:=Touch;

  {Start Touch}
  Status:=TouchDeviceStart(@Touch.Touch);
  if Status <> ERROR_SUCCESS then
   begin
    if HID_LOG_ENABLED then HIDLogError(Device,'Touch: Failed to start new touch device: ' + ErrorToString(Status));

    Exit;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  if Result <> ERROR_SUCCESS then
   begin
    {Update Collection}
    Collection.PrivateData:=nil;

    {Deregister Touch}
    TouchDeviceDeregister(@Touch.Touch);

    {Cancel Reports / Release Reports / Free Definitions}
    Definition:=Touch.Definitions;
    while Definition <> nil do
     begin
      {Unlink Definition}
      Touch.Definitions:=Definition.Next;

      {Cancel Report}
      HIDDeviceCancelReport(Device,Definition.Id);

      {Release Report}
      HIDDeviceReleaseReport(Device,Definition.Id);

      {Free Definitions}
      HIDFreeDefinition(Definition);

      {Get Next Definition}
      Definition:=Touch.Definitions;
     end;

    {Destroy Touch}
    TouchDeviceDestroy(@Touch.Touch);
   end;
 end;
end;

{==============================================================================}

function HIDTouchCollectionUnbind(Device:PHIDDevice;Collection:PHIDCollection):LongWord;
{Implementation of HIDCollectionUnbind API for HID Touch}
{Note: Not intended to be called directly by applications}
var
 Touch:PHIDTouchDevice;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Collection}
 if Collection = nil then Exit;

 {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Device,'Touch: Unbinding HID collection (Name=' + DeviceGetName(@Device.Device) + ' Description=' + Device.Device.DeviceDescription + ' Usage=' + IntToHex(Collection.Page,4) + IntToHex(Collection.Usage,4) + ')');
 {$ENDIF}

 {Get Touch}
 Touch:=PHIDTouchDevice(Collection.PrivateData);
 if Touch = nil then Exit;
 if Touch.Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Stop Touch}
 if TouchDeviceStop(@Touch.Touch) <> ERROR_SUCCESS then Exit;

 {Update Collection}
 Collection.PrivateData:=nil;

 {Deregister Touch}
 if TouchDeviceDeregister(@Touch.Touch) <> ERROR_SUCCESS then Exit;

 {Release Reports / Free Definitions}
 Definition:=Touch.Definitions;
 while Definition <> nil do
  begin
   {Unlink Definition}
   Touch.Definitions:=Definition.Next;

   {Release Report}
   HIDDeviceReleaseReport(Device,Definition.Id);

   {Free Definitions}
   HIDFreeDefinition(Definition);

   {Get Next Definition}
   Definition:=Touch.Definitions;
  end;

 {Destroy Touch}
 TouchDeviceDestroy(@Touch.Touch);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDTouchReportReceive(Collection:PHIDCollection;ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord;
{Implementation of HIDReportReceive API for HID Touch}
{Note: Not intended to be called directly by applications}
var
 Temp:LongWord;
 Count:LongWord;
 Total:LongWord;
 Status:LongWord;

 Touch:PHIDTouchDevice;
 TouchData:TTouchData;
 MouseData:TMouseData;

 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Collection}
 if Collection = nil then Exit;

 {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Touch: Report received (Id=' + IntToStr(ReportId) + ' Size=' + IntToStr(ReportSize) + ')');
 {$ENDIF}

 {Get Touch}
 Touch:=PHIDTouchDevice(Collection.PrivateData);
 if Touch <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Touch.Touch.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Touch.Touch.ReceiveCount);

      {Disable Timer}
      TimerDisable(Touch.Timer);

      {Check State}
      if Touch.Touch.TouchState <> TOUCH_STATE_ENABLED then
       begin
        {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
        if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Touch: Device disabled, report discarded');
        {$ENDIF}

        Result:=ERROR_SUCCESS;
        Exit;
       end;

      {Get Definition}
      Definition:=Touch.Definitions;
      while Definition <> nil do
       begin
        {Check Report Id}
        if Definition.Id = ReportId then Break;

        {Get Next Definition}
        Definition:=Definition.Next;
       end;

      {Check Result}
      if (Definition <> nil) and (ReportSize >= Definition.Size) then
       begin
        {Set Defaults}
        Total:=0;

        {Clear Data}
        FillChar(TouchData,SizeOf(TTouchData),0);
        FillChar(MouseData,SizeOf(TMouseData),0);

        {Extract Report}
        Status:=HIDTouchExtractReport(Touch,Definition,ReportData,ReportSize,Total);
        if Status = ERROR_SUCCESS then
         begin
          if Total > 0 then
           begin
            {Check First Identifier}
            if Touch.FirstIdentifier = INFINITE then
             begin
              Touch.FirstIdentifier:=Touch.TouchPoints[0].Identifier;
             end;

            for Count:=0 to Total - 1 do
             begin
              {Check Swap}
              if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_SWAP_XY) <> 0 then
               begin
                {Swap X/Y}
                Temp:=Touch.TouchPoints[Count].X;
                Touch.TouchPoints[Count].X:=Touch.TouchPoints[Count].Y;
                Touch.TouchPoints[Count].Y:=Temp;
               end;

              {Check Invert}
              if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_INVERT_X) <> 0 then
               begin
                {Invert X}
                Touch.TouchPoints[Count].X:=Touch.MaxX - Touch.TouchPoints[Count].X;
               end;
              if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_INVERT_Y) <> 0 then
               begin
                {Invert Y}
                Touch.TouchPoints[Count].Y:=Touch.MaxY - Touch.TouchPoints[Count].Y;
               end;

              {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
              if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Touch:  Id=' + IntToStr(Touch.TouchPoints[Count].Identifier) + ' X=' + IntToStr(Touch.TouchPoints[Count].X) + ' Y=' + IntToStr(Touch.TouchPoints[Count].Y) + ' Tip=' + BoolToStr(Touch.TouchPoints[Count].TipSwitch,True));
              {$ENDIF}

              {Check Flags}
              if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_MOUSE_DATA) = 0 then
               begin
                {For touch report all points}
                {Create Touch Data}
                TouchData.Info:=0;
                if Touch.TouchPoints[Count].TipSwitch then TouchData.Info:=TOUCH_FINGER;
                TouchData.PointID:=Touch.TouchPoints[Count].Identifier + 1 - Touch.FirstIdentifier;

                {Check Rotation}
                case Touch.Touch.Properties.Rotation of
                 TOUCH_ROTATION_0:begin
                   {No Change}
                   TouchData.PositionX:=Touch.TouchPoints[Count].X;
                   TouchData.PositionY:=Touch.TouchPoints[Count].Y;
                  end;
                 TOUCH_ROTATION_90:begin
                   {Swap X and Y, Invert Y}
                   TouchData.PositionX:=Touch.TouchPoints[Count].Y;
                   TouchData.PositionY:=Touch.Touch.Properties.MaxY - Touch.TouchPoints[Count].X;
                  end;
                 TOUCH_ROTATION_180:begin
                   {Invert X and Y}
                   TouchData.PositionX:=Touch.Touch.Properties.MaxX - Touch.TouchPoints[Count].X;
                   TouchData.PositionY:=Touch.Touch.Properties.MaxY - Touch.TouchPoints[Count].Y;
                  end;
                 TOUCH_ROTATION_270:begin
                   {Swap X and Y, Invert X}
                   TouchData.PositionX:=Touch.Touch.Properties.MaxX - Touch.TouchPoints[Count].Y;
                   TouchData.PositionY:=Touch.TouchPoints[Count].X;
                  end;
                end;
                TouchData.PositionZ:=Touch.TouchPoints[Count].Pressure;

                {Check Rotation}
                case Touch.Touch.Properties.Rotation of
                 TOUCH_ROTATION_0,TOUCH_ROTATION_180:begin
                   {No Change}
                   TouchData.TouchWidth:=Touch.TouchPoints[Count].Width;
                   TouchData.TouchHeight:=Touch.TouchPoints[Count].Height;
                  end;
                 TOUCH_ROTATION_90,TOUCH_ROTATION_270:begin
                   {Swap Width and Height}
                   TouchData.TouchWidth:=Touch.TouchPoints[Count].Height;
                   TouchData.TouchHeight:=Touch.TouchPoints[Count].Width;
                  end;
                end;

                {Check Event}
                if Assigned(Touch.Touch.Event) then
                 begin
                  {Event Parameter}
                  TouchData.Parameter:=Touch.Touch.Parameter;

                  {Event Callback}
                  Touch.Touch.Event(@Touch.Touch,@TouchData);
                 end
                else
                 begin
                  {Insert Data}
                  TouchInsertData(@Touch.Touch,@TouchData,True);
                 end;
               end
              else
               begin
                {For mouse report the first point}
                if Touch.TouchPoints[Count].Identifier = Touch.FirstIdentifier then
                 begin
                  {Create Mouse Data}
                  MouseData.Buttons:=MOUSE_ABSOLUTE_X or MOUSE_ABSOLUTE_Y; {Absolute X and Y}
                  if Touch.TouchPoints[Count].TipSwitch then MouseData.Buttons:=MouseData.Buttons or MOUSE_TOUCH_BUTTON; {Touch Button}

                  {Check Rotation}
                  case Touch.Touch.Properties.Rotation of
                   TOUCH_ROTATION_0:begin
                     {No Change}
                     MouseData.OffsetX:=Touch.TouchPoints[Count].X;
                     MouseData.OffsetY:=Touch.TouchPoints[Count].Y;
                    end;
                   TOUCH_ROTATION_90:begin
                     {Swap X and Y, Invert Y}
                     MouseData.OffsetX:=Touch.TouchPoints[Count].Y;
                     MouseData.OffsetY:=Touch.Touch.Properties.MaxY - Touch.TouchPoints[Count].X;
                    end;
                   TOUCH_ROTATION_180:begin
                     {Invert X and Y}
                     MouseData.OffsetX:=Touch.Touch.Properties.MaxX - Touch.TouchPoints[Count].X;
                     MouseData.OffsetY:=Touch.Touch.Properties.MaxY - Touch.TouchPoints[Count].Y;
                    end;
                   TOUCH_ROTATION_270:begin
                     {Swap X and Y, Invert X}
                     MouseData.OffsetX:=Touch.Touch.Properties.MaxX - Touch.TouchPoints[Count].Y;
                     MouseData.OffsetY:=Touch.TouchPoints[Count].X;
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
                    if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Touch: Failed to write mouse data, packet discarded');

                    {Update Statistics}
                    Inc(Touch.Touch.ReceiveErrors);
                   end;
                 end;
               end;
             end;

            {Save Last Touch Points}
            System.Move(Touch.TouchPoints[0],Touch.LastPoints[0],Total * SizeOf(THIDTouchPoint));
            Touch.LastCount:=Total;
           end;
         end
        else
         begin
          if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Touch: Failed report request, unable to extract report data (Id=' + IntToStr(ReportId) + ' Size=' + IntToStr(ReportSize) + ')');

          {Update Statistics}
          Inc(Touch.Touch.ReceiveErrors);
         end;
       end
      else
       begin
        if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Touch: Failed report request, no report definition or invalid size (Id=' + IntToStr(ReportId) + ' Size=' + IntToStr(ReportSize) + ')');

        {Update Statistics}
        Inc(Touch.Touch.ReceiveErrors);
       end;

      {Enable Timer}
      TimerEnable(Touch.Timer);

      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Touch.Touch.Lock);
     end;
    end
   else
    begin
     if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Touch: Failed to acquire lock');
    end;
  end
 else
  begin
   if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Touch: Report request invalid');
  end;
end;

{==============================================================================}

procedure HIDTouchTimer(Touch:PHIDTouchDevice);
{Touch point release timer event handler for HID touch}
{Note: Not intended to be called directly by applications}
var
 Count:LongWord;

 TouchData:TTouchData;
 MouseData:TMouseData;
begin
 {}
 {Check Touch}
 if Touch = nil then Exit;
 if Touch.LastPoints = nil then Exit;

 {Acquire the Lock}
 if MutexLock(Touch.Touch.Lock) = ERROR_SUCCESS then
  begin
   try
    {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
    if HID_LOG_ENABLED then HIDLogDebug(Touch.Collection.Device,'Touch: Touch Timer');
    {$ENDIF}

    if Touch.LastCount > 0 then
     begin
      {Clear Data}
      FillChar(TouchData,SizeOf(TTouchData),0);
      FillChar(MouseData,SizeOf(TMouseData),0);

      {Send release events for any touch points that the device failed to notify}
      for Count:=0 to Touch.LastCount - 1 do
       begin
        if Touch.LastPoints[Count].TipSwitch then
         begin
          {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
          if HID_LOG_ENABLED then HIDLogDebug(Touch.Collection.Device,'Touch:  Release Id=' + IntToStr(Touch.LastPoints[Count].Identifier) + ' X=' + IntToStr(Touch.LastPoints[Count].X) + ' Y=' + IntToStr(Touch.LastPoints[Count].Y));
          {$ENDIF}

          {Check Flags}
          if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_MOUSE_DATA) = 0 then
           begin
            {For touch report all points}
            {Create Touch Data}
            TouchData.Info:=0;
            TouchData.PointID:=Touch.LastPoints[Count].Identifier + 1;

            {Check Rotation}
            case Touch.Touch.Properties.Rotation of
             TOUCH_ROTATION_0:begin
               {No Change}
               TouchData.PositionX:=Touch.LastPoints[Count].X;
               TouchData.PositionY:=Touch.LastPoints[Count].Y;
              end;
             TOUCH_ROTATION_90:begin
               {Swap X and Y, Invert Y}
               TouchData.PositionX:=Touch.LastPoints[Count].Y;
               TouchData.PositionY:=Touch.Touch.Properties.MaxY - Touch.LastPoints[Count].X;
              end;
             TOUCH_ROTATION_180:begin
               {Invert X and Y}
               TouchData.PositionX:=Touch.Touch.Properties.MaxX - Touch.LastPoints[Count].X;
               TouchData.PositionY:=Touch.Touch.Properties.MaxY - Touch.LastPoints[Count].Y;
              end;
             TOUCH_ROTATION_270:begin
               {Swap X and Y, Invert X}
               TouchData.PositionX:=Touch.Touch.Properties.MaxX - Touch.LastPoints[Count].Y;
               TouchData.PositionY:=Touch.LastPoints[Count].X;
              end;
            end;
            TouchData.PositionZ:=Touch.LastPoints[Count].Pressure;

            {Check Rotation}
            case Touch.Touch.Properties.Rotation of
             TOUCH_ROTATION_0,TOUCH_ROTATION_180:begin
               {No Change}
               TouchData.TouchWidth:=Touch.LastPoints[Count].Width;
               TouchData.TouchHeight:=Touch.LastPoints[Count].Height;
              end;
             TOUCH_ROTATION_90,TOUCH_ROTATION_270:begin
               {Swap Width and Height}
               TouchData.TouchWidth:=Touch.LastPoints[Count].Height;
               TouchData.TouchHeight:=Touch.LastPoints[Count].Width;
              end;
            end;

            {Check Event}
            if Assigned(Touch.Touch.Event) then
             begin
              {Event Parameter}
              TouchData.Parameter:=Touch.Touch.Parameter;

              {Event Callback}
              Touch.Touch.Event(@Touch.Touch,@TouchData);
             end
            else
             begin
              {Insert Data}
              TouchInsertData(@Touch.Touch,@TouchData,True);
             end;
           end
          else
           begin
            {For mouse report the first point}
            if Touch.LastPoints[Count].Identifier = 0 then
             begin
              {Create Mouse Data}
              MouseData.Buttons:=MOUSE_ABSOLUTE_X or MOUSE_ABSOLUTE_Y; {Absolute X and Y}

              {Check Rotation}
              case Touch.Touch.Properties.Rotation of
               TOUCH_ROTATION_0:begin
                 {No Change}
                 MouseData.OffsetX:=Touch.LastPoints[Count].X;
                 MouseData.OffsetY:=Touch.LastPoints[Count].Y;
                end;
               TOUCH_ROTATION_90:begin
                 {Swap X and Y, Invert Y}
                 MouseData.OffsetX:=Touch.LastPoints[Count].Y;
                 MouseData.OffsetY:=Touch.Touch.Properties.MaxY - Touch.LastPoints[Count].X;
                end;
               TOUCH_ROTATION_180:begin
                 {Invert X and Y}
                 MouseData.OffsetX:=Touch.Touch.Properties.MaxX - Touch.LastPoints[Count].X;
                 MouseData.OffsetY:=Touch.Touch.Properties.MaxY - Touch.LastPoints[Count].Y;
                end;
               TOUCH_ROTATION_270:begin
                 {Swap X and Y, Invert X}
                 MouseData.OffsetX:=Touch.Touch.Properties.MaxX - Touch.LastPoints[Count].Y;
                 MouseData.OffsetY:=Touch.LastPoints[Count].X;
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
                if HID_LOG_ENABLED then HIDLogError(Touch.Collection.Device,'Touch: Failed to write mouse data, packet discarded');

                {Update Statistics}
                Inc(Touch.Touch.ReceiveErrors);
               end;
             end;
           end;
         end;
       end;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Touch.Touch.Lock);
   end;
  end
 else
  begin
   if HID_LOG_ENABLED then HIDLogError(Touch.Collection.Device,'Touch: Failed to acquire lock');
  end;
end;

{==============================================================================}

function HIDTouchGetMaxXYZ(Touch:PHIDTouchDevice;var MaxX,MaxY,MaxZ,MaxWidth,MaxHeight:LongWord):LongWord;
{Scan the allocated HID input definitions to determine the maximum X, Y, Z, Width and Height values}
{Note: Not intended to be called directly by applications}
var
 Field:PHIDField;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Defaults}
 MaxX:=0;
 MaxY:=0;
 MaxZ:=0;
 MaxWidth:=0;
 MaxHeight:=0;

 {Check Touch}
 if Touch = nil then Exit;

 {Check Definitions}
 Definition:=Touch.Definitions;
 while Definition <> nil do
  begin
   {Check Fields}
   Field:=Definition.Fields;
   while Field <> nil do
    begin
     {Check Page}
     case Field.Page of
      HID_PAGE_GENERIC_DESKTOP:begin
        {Check Usage}
        case Field.Usage of
         HID_DESKTOP_X:begin
           {Max X}
           if Field.Logical.Maximum > MaxX then
            begin
             MaxX:=Field.Logical.Maximum;
            end;
          end;
         HID_DESKTOP_Y:begin
           {Max Y}
           if Field.Logical.Maximum > MaxY then
            begin
             MaxY:=Field.Logical.Maximum;
            end;
          end;
        end;
       end;
      HID_PAGE_DIGITIZERS:begin
        {Check Usage}
        case Field.Usage of
         HID_DIGITIZERS_TIP_PRESSURE:begin
           {Max Z}
           if Field.Logical.Maximum > MaxZ then
            begin
             MaxZ:=Field.Logical.Maximum;
            end;
          end;
         HID_DIGITIZERS_WIDTH:begin
           {Max Width}
           if Field.Logical.Maximum > MaxWidth then
            begin
             MaxWidth:=Field.Logical.Maximum;
            end;
          end;
         HID_DIGITIZERS_HEIGHT:begin
           {Max Height}
           if Field.Logical.Maximum > MaxHeight then
            begin
             MaxHeight:=Field.Logical.Maximum;
            end;
          end;
        end;
       end;
     end;

     {Get Next Field}
     Field:=Field.Next;
    end;

   {Get Next Definition}
   Definition:=Definition.Next;
  end;

 {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Touch.Collection.Device,'Touch: Maximum X=' + IntToStr(MaxX) + ' Y=' + IntToStr(MaxY) + ' Z=' + IntToStr(MaxZ));
 {$ENDIF}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDTouchGetMaxPoints(Touch:PHIDTouchDevice;var MaxPoints:LongWord):LongWord;
{Request the HID feature report containing the maximum contact count (if available)}
{Note: Not intended to be called directly by applications}
var
 Data:PByte;
 Size:LongWord;
 Field:PHIDField;
 Device:PHIDDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Default}
 MaxPoints:=1;

 {Check Touch}
 if Touch = nil then Exit;

 {Get Device}
 Device:=PHIDDevice(Touch.Touch.Device.DeviceData);
 if Device = nil then Exit;

 Result:=ERROR_NOT_SUPPORTED;

 {Check Definition}
 if Touch.FeatureDefinition = nil then Exit;

 Result:=ERROR_OUTOFMEMORY;

 {Allocate Data}
 Size:=Touch.FeatureDefinition.Size;
 Data:=AllocMem(Size);
 if Data = nil then Exit;
 try
  {Get Report}
  Result:=HIDDeviceGetReport(Device,Touch.FeatureDefinition.Kind,Touch.FeatureDefinition.Id,Data,Size);
  if Result <> ERROR_SUCCESS then Exit;

  {Extract Fields}
  Field:=Touch.FeatureDefinition.Fields;
  while Field <> nil do
   begin
    {Check Page}
    case Field.Page of
     HID_PAGE_DIGITIZERS:begin
       {Check Usage}
       case Field.Usage of
        {Contact Count Maximum}
        HID_DIGITIZERS_CONTACT_COUNT_MAXIMUM:begin
          HIDExtractUnsignedField(Field,Data,Size,MaxPoints);
         end;
       end;
      end;
    end;

   {Get Next Field}
   Field:=Field.Next;
  end;

  {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
  if HID_LOG_ENABLED then HIDLogDebug(Touch.Collection.Device,'Touch: Contact Count Maximum=' + IntToStr(MaxPoints));
  {$ENDIF}

  Result:=ERROR_SUCCESS;
 finally
  {Free Data}
  FreeMem(Data);
 end;
end;

{==============================================================================}

function HIDTouchExtractReport(Touch:PHIDTouchDevice;Definition:PHIDDefinition;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Process the received HID report against the matching HID definition and extract all available fields}
{Note: Not intended to be called directly by applications}
var
 Index:LongWord;

 Field:PHIDField;

 IndexX:LongWord;
 IndexY:LongWord;
 IndexTip:LongWord;
 IndexRange:LongWord;
 IndexWidth:LongWord;
 IndexHeight:LongWord;
 IndexPressure:LongWord;
 IndexConfidence:LongWord;
 IndexIdentifier:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Default}
 Count:=0;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.TouchPoints = nil then Exit;

 {Check Definition}
 if Definition = nil then Exit;

 {Check Data}
 if Data = nil then Exit;

 {Check Size}
 if Size < Definition.Size then Exit;

 {Get Contact Count}
 Field:=Definition.Fields;
 while Field <> nil do
  begin
   {Check Page}
   case Field.Page of
    HID_PAGE_DIGITIZERS:begin
      {Check Usage}
      case Field.Usage of
       HID_DIGITIZERS_CONTACT_COUNT:begin
         {Contact Count}
         HIDExtractUnsignedField(Field,Data,Size,Count);
        end;
      end;
     end;
   end;

   {Get Next Field}
   Field:=Field.Next;
  end;

 {Check Contact Count}
 if Count > Touch.MaxPoints then Count:=Touch.MaxPoints;

 {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Touch.Collection.Device,'Touch: Contact Count=' + IntToStr(Count));
 {$ENDIF}

 {Set Defaults}
 IndexX:=0;
 IndexY:=0;
 IndexTip:=0;
 IndexRange:=0;
 IndexWidth:=0;
 IndexHeight:=0;
 IndexPressure:=0;
 IndexConfidence:=0;
 IndexIdentifier:=0;

 {Extract Fields}
 Field:=Definition.Fields;
 while Field <> nil do
  begin
   {Check Page}
   case Field.Page of
    HID_PAGE_GENERIC_DESKTOP:begin
      {Check Usage}
      case Field.Usage of
       HID_DESKTOP_X:begin
         {X}
         if IndexX < Count then
          begin
           HIDExtractUnsignedField(Field,Data,Size,Touch.TouchPoints[IndexX].X);

           Inc(IndexX);
          end;
        end;
       HID_DESKTOP_Y:begin
         {Y}
         if IndexY < Count then
          begin
           HIDExtractUnsignedField(Field,Data,Size,Touch.TouchPoints[IndexY].Y);

           Inc(IndexY);
          end;
        end;
      end;
     end;
    HID_PAGE_DIGITIZERS:begin
      {Check Usage}
      case Field.Usage of
       HID_DIGITIZERS_TIP_PRESSURE:begin
         {Tip Pressure}
         if IndexPressure < Count then
          begin
           HIDExtractUnsignedField(Field,Data,Size,Touch.TouchPoints[IndexPressure].Pressure);

           Inc(IndexPressure);
          end;
        end;
       HID_DIGITIZERS_IN_RANGE:begin
         {In Range}
         if IndexRange < Count then
          begin
           HIDExtractBitField(Field,Data,Size,Touch.TouchPoints[IndexRange].InRange);

           Inc(IndexRange);
          end;
        end;
       HID_DIGITIZERS_TIP_SWITCH:begin
         {Tip Switch}
         if IndexTip < Count then
          begin
           HIDExtractBitField(Field,Data,Size,Touch.TouchPoints[IndexTip].TipSwitch);

           Inc(IndexTip);
          end;
        end;
       HID_DIGITIZERS_TOUCH_VALID:begin
         {Touch Valid (Confidence)}
         if IndexConfidence < Count then
          begin
           HIDExtractBitField(Field,Data,Size,Touch.TouchPoints[IndexConfidence].Confidence);

           Inc(IndexConfidence);
          end;
        end;
       HID_DIGITIZERS_WIDTH:begin
         {Width}
         if IndexWidth < Count then
          begin
           HIDExtractUnsignedField(Field,Data,Size,Touch.TouchPoints[IndexWidth].Width);

           Inc(IndexWidth);
          end;
        end;
       HID_DIGITIZERS_HEIGHT:begin
         {Height}
         if IndexHeight < Count then
          begin
           HIDExtractUnsignedField(Field,Data,Size,Touch.TouchPoints[IndexHeight].Height);

           Inc(IndexHeight);
          end;
        end;
       HID_DIGITIZERS_CONTACT_IDENTIFIER:begin
         {Contact Identifier}
         if IndexIdentifier < Count then
          begin
           HIDExtractUnsignedField(Field,Data,Size,Touch.TouchPoints[IndexIdentifier].Identifier);

           Inc(IndexIdentifier);
          end;
        end;
      end;
     end;
   end;

   {Get Next Field}
   Field:=Field.Next;
  end;

 {$IF DEFINED(TOUCH_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then
  begin
   for Index:=0 to Count - 1 do
    begin
     HIDLogDebug(Touch.Collection.Device,'Touch: Report data (Id=' + IntToStr(Touch.TouchPoints[Index].Identifier) + ' X=' + IntToStr(Touch.TouchPoints[Index].X) + ' Y=' + IntToStr(Touch.TouchPoints[Index].Y) + ')');
     HIDLogDebug(Touch.Collection.Device,'                   (Tip=' + BoolToStr(Touch.TouchPoints[Index].TipSwitch,True) + ' Width=' + IntToStr(Touch.TouchPoints[Index].Width) + ' Height=' + IntToStr(Touch.TouchPoints[Index].Height) + ')');
    end;
  end;
 {$ENDIF}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}

initialization
 HIDTouchInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.


