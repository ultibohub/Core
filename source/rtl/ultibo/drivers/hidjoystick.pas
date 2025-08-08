{
Ultibo HID Joystick and Gamepad consumer unit.

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


HID Joystick and Gamepad
========================

 This is a consumer for any generic HID joystick or gamepad device, it accepts HID
 application collections in the generic desktop page (HID_PAGE_GENERIC_DESKTOP) with
 the usage set to joystick (HID_DESKTOP_JOYSTICK) or gamepad (HID_DESKTOP_GAMEPAD).

 The consumer will bind to any joystick or gamepad collection that implements at a
 minimum the X and Y axis and at least one button.

 This consumer doesn't support certain well known gamepad devices such as the Xbox
 or PlayStation controllers that have specific control and button mappings and may
 require special command sequences to enable certain features. The option to create
 an additional driver that has specific support for some of those devices is being
 considered.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit HIDJoystick;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  HID,
  Joystick,
  SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {HID Joystick specific constants}
 HID_JOYSTICK_CONSUMER_NAME = 'HID Joystick and Gamepad Consumer'; {Name of HID Joystick consumer}

 HID_JOYSTICK_DESCRIPTION = 'HID Joystick'; {Description of HID Joystick device}
 HID_GAMEPAD_DESCRIPTION = 'HID Gamepad';   {Description of HID Gamepad device}

{==============================================================================}
type
 {HID Joystick specific types}
 PHIDJoystickDevice = ^THIDJoystickDevice;
 THIDJoystickDevice = record
  {Joystick Properties}
  Joystick:TJoystickDevice;
  {HID Properties}
  Collection:PHIDCollection;   {The HID collection this joystick is bound to}
  Definitions:PHIDDefinition;  {The input report definitions that can be accepted as joystick reports}
 end;

{==============================================================================}
{var}
 {HID Joystick specific variables}

{==============================================================================}
{Initialization Functions}
procedure HIDJoystickInit;

{==============================================================================}
{HID Joystick Functions}
function HIDJoystickStart(Joystick:PJoystickDevice):LongWord;
function HIDJoystickStop(Joystick:PJoystickDevice):LongWord;

function HIDJoystickUpdate(Joystick:PJoystickDevice):LongWord;

{==============================================================================}
{HID Joystick Helper Functions}
function HIDJoystickCheckCollection(Collection:PHIDCollection):LongWord;
function HIDJoystickCheckDefinition(Definition:PHIDDefinition):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {HID Joystick specific variables}
 HIDJoystickInitialized:Boolean;

 HIDJoystickConsumer:PHIDConsumer; {HID Consumer interface (Set by HIDJoystickInit)}

{==============================================================================}
{==============================================================================}
{Forward Declarations}
function HIDJoystickCollectionBind(Device:PHIDDevice;Collection:PHIDCollection):LongWord; forward;
function HIDJoystickCollectionUnbind(Device:PHIDDevice;Collection:PHIDCollection):LongWord; forward;

function HIDJoystickReportReceive(Collection:PHIDCollection;ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord; forward;

function HIDJoystickGetCounts(Joystick:PHIDJoystickDevice;var HatCount,AxisCount,ButtonCount:LongWord):LongWord; forward;
function HIDJoystickGetControls(Joystick:PHIDJoystickDevice;HatCount,AxisCount,ButtonCount:LongWord):LongWord; forward;

function HIDJoystickExtractReport(Joystick:PHIDJoystickDevice;Definition:PHIDDefinition;Buffer:Pointer;Size:LongWord;Data:PJoystickData):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure HIDJoystickInit;
{Initialize the HID Joystick unit and HID Joystick driver}

{Note: Called only during system startup}
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if HIDJoystickInitialized then Exit;

 if HID_REGISTER_JOYSTICK then
  begin
   {Create HID Joystick Consumer}
   HIDJoystickConsumer:=HIDConsumerCreate;
   if HIDJoystickConsumer <> nil then
    begin
     {Update HID Joystick Consumer}
     {Driver}
     HIDJoystickConsumer.Driver.DriverName:=HID_JOYSTICK_CONSUMER_NAME;
     {HID}
     HIDJoystickConsumer.CollectionBind:=HIDJoystickCollectionBind;
     HIDJoystickConsumer.CollectionUnbind:=HIDJoystickCollectionUnbind;
     HIDJoystickConsumer.ReportReceive:=HIDJoystickReportReceive;

     {Register HID Joystick Consumer}
     Status:=HIDConsumerRegister(HIDJoystickConsumer);
     if Status <> ERROR_SUCCESS then
      begin
       if HID_LOG_ENABLED then HIDLogError(nil,'Joystick: Failed to register HID Joystick and Gamepad consumer: ' + ErrorToString(Status));

       {Destroy Consumer}
       HIDConsumerDestroy(HIDJoystickConsumer);

       HIDJoystickConsumer:=nil;
      end;
    end
   else
    begin
     if HID_LOG_ENABLED then HIDLogError(nil,'Joystick: Failed to create HID Joystick and Gamepad consumer');
    end;
  end;

 HIDJoystickInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{HID Joystick Functions}
function HIDJoystickStart(Joystick:PJoystickDevice):LongWord;
{Implementation of JoystickDeviceStart API for HID Joystick device}
{Note: Not intended to be called directly by applications, use JoystickDeviceStart instead}
var
 Status:LongWord;
 Device:PHIDDevice;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;

 {$IF DEFINED(JOYSTICK_DEBUG) or DEFINED(HID_DEBUG)}
 if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'HID: Joystick Start');
 {$ENDIF}

 {Get Device}
 Device:=PHIDDevice(Joystick.Device.DeviceData);
 if Device = nil then Exit;

 {Update Configuration}
 Status:=HIDJoystickUpdate(Joystick);
 if Status <> ERROR_SUCCESS then
  begin
   if HID_LOG_ENABLED then HIDLogError(Device,'Joystick: Failed to update configuration: ' + ErrorToString(Status));

   Result:=Status;
   Exit;
  end;

 {Submit Reports}
 Definition:=PHIDJoystickDevice(Joystick).Definitions;
 while Definition <> nil do
  begin
   Status:=HIDDeviceSubmitReport(Device,Definition.Id);
   if Status <> ERROR_SUCCESS then
    begin
     if HID_LOG_ENABLED then HIDLogError(Device,'Joystick: Failed to submit report id ' + IntToStr(Definition.Id) + ': ' + ErrorToString(Status));

     Result:=Status;
     Exit;
    end;

   {Get Next Definition}
   Definition:=Definition.Next;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDJoystickStop(Joystick:PJoystickDevice):LongWord;
{Implementation of JoystickDeviceStop API for HID Joystick device}
{Note: Not intended to be called directly by applications, use JoystickDeviceStop instead}
var
 Device:PHIDDevice;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;

 {$IF DEFINED(JOYSTICK_DEBUG) or DEFINED(HID_DEBUG)}
 if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'HID: Joystick Stop');
 {$ENDIF}

 {Get Device}
 Device:=PHIDDevice(Joystick.Device.DeviceData);
 if Device = nil then Exit;

 {Cancel Reports}
 Definition:=PHIDJoystickDevice(Joystick).Definitions;
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

function HIDJoystickUpdate(Joystick:PJoystickDevice):LongWord;
{Implementation of JoystickDeviceUpdate API for HID Joystick device}
{Note: Not intended to be called directly by applications, use JoystickDeviceUpdate instead}
var
 HatCount:LongWord;
 AxisCount:LongWord;
 ButtonCount:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;

 {$IF DEFINED(JOYSTICK_DEBUG) or DEFINED(HID_DEBUG)}
 if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'HID: Joystick Update');
 {$ENDIF}

 {Acquire Lock}
 if MutexLock(Joystick.Lock) = ERROR_SUCCESS then
  begin
   try
    {Set Defaults}
    HatCount:=0;
    AxisCount:=0;
    ButtonCount:=0;

    {Get Counts}
    Result:=HIDJoystickGetCounts(PHIDJoystickDevice(Joystick),HatCount,AxisCount,ButtonCount);
    if Result <> ERROR_SUCCESS then Exit;

    {Check Counts}
    if (HatCount <> Joystick.Properties.HatCount) or (AxisCount <> Joystick.Properties.AxisCount) or (ButtonCount <> Joystick.Properties.ButtonCount) then
     begin
      {Get Controls}
      Result:=HIDJoystickGetControls(PHIDJoystickDevice(Joystick),HatCount,AxisCount,ButtonCount);
      if Result <> ERROR_SUCCESS then Exit;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    MutexUnlock(Joystick.Lock);
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
{HID Joystick Helper Functions}
function HIDJoystickCheckCollection(Collection:PHIDCollection):LongWord;
{Check if a HID collection is suitable for use as a joystick device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Collection}
 if Collection = nil then Exit;

 {Set Result}
 Result:=ERROR_NOT_SUPPORTED;

 {Check Page}
 case Collection.Page of
  HID_PAGE_GENERIC_DESKTOP:begin
    {Check Usage}
    case Collection.Usage of
     HID_DESKTOP_JOYSTICK,
     HID_DESKTOP_GAMEPAD:begin
       {Check Flags}
       if Collection.Flags = HID_MAIN_COLLECTION_APPLICATION then Result:=ERROR_SUCCESS;
      end;
    end;
   end;
 end;
end;

{==============================================================================}

function HIDJoystickCheckDefinition(Definition:PHIDDefinition):LongWord;
{Check if a HID definition is suitable for use as a joystick input report}
var
 HasX:Boolean;
 HasY:Boolean;
 HasButton:Boolean;
 Field:PHIDField;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Definition}
 if Definition = nil then Exit;

 {Set Defaults}
 HasX:=False;
 HasY:=False;
 HasButton:=False;

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
    HID_PAGE_BUTTON:begin
      {Check Button}
      if (Field.Usage >= HID_BUTTON_PRIMARY) and (Field.Usage + Field.Count - 1 <= HID_BUTTON_65535) then
       begin
        HasButton:=True;
       end;
     end;
   end;

   {Get Next Field}
   Field:=Field.Next;
  end;

 {Check Result}
 if HasX and HasY and HasButton then Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{HID Joystick Internal Functions}
function HIDJoystickCollectionBind(Device:PHIDDevice;Collection:PHIDCollection):LongWord;
{Implementation of HIDCollectionBind API for HID Joystick}
{Note: Not intended to be called directly by applications}
var
 MinId:Byte;
 MaxId:Byte;
 Index:LongWord;
 Status:LongWord;
 Joystick:PHIDJoystickDevice;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Collection}
 if Collection = nil then Exit;

 {$IF DEFINED(JOYSTICK_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Device,'Joystick: Attempting to bind HID collection (Name=' + DeviceGetName(@Device.Device) + ' Description=' + Device.Device.DeviceDescription + ' Usage=' + IntToHex(Collection.Page,4) + IntToHex(Collection.Usage,4) + ')');
 {$ENDIF}

 {Set Result}
 Result:=ERROR_NOT_SUPPORTED;

 {Check Collection}
 Status:=HIDJoystickCheckCollection(Collection);
 if Status <> ERROR_SUCCESS then Exit;

 {Create Joystick}
 Joystick:=PHIDJoystickDevice(JoystickDeviceCreateEx(SizeOf(THIDJoystickDevice)));
 if Joystick = nil then
  begin
   if HID_LOG_ENABLED then HIDLogError(Device,'Joystick: Failed to create new joystick device');

   Exit;
  end;

 {Update Joystick}
 {Device}
 Joystick.Joystick.Device.DeviceBus:=DEVICE_BUS_NONE;
 Joystick.Joystick.Device.DeviceType:=JOYSTICK_TYPE_JOYSTICK;
 if Device.Device.DeviceType = HID_TYPE_USB then
  begin
   Joystick.Joystick.Device.DeviceBus:=DEVICE_BUS_USB;
  end;
 if Collection.Usage = HID_DESKTOP_GAMEPAD then
  begin
   Joystick.Joystick.Device.DeviceType:=JOYSTICK_TYPE_GAMEPAD;
  end;
 Joystick.Joystick.Device.DeviceFlags:=Joystick.Joystick.Device.DeviceFlags; {Don't override defaults (was JOYSTICK_FLAG_NONE)}
 Joystick.Joystick.Device.DeviceData:=Device;
 Joystick.Joystick.Device.DeviceDescription:=Device.Device.DeviceDescription; {HID_JOYSTICK_DESCRIPTION / HID_GAMEPAD_DESCRIPTION}
 {Joystick}
 Joystick.Joystick.JoystickState:=JOYSTICK_STATE_DISABLED;
 Joystick.Joystick.DeviceStart:=HIDJoystickStart;
 Joystick.Joystick.DeviceStop:=HIDJoystickStop;
 Joystick.Joystick.DeviceUpdate:=HIDJoystickUpdate;
 {Driver}
 Joystick.Joystick.Properties.Flags:=Joystick.Joystick.Device.DeviceFlags;
 Joystick.Joystick.Properties.AxisCount:=0;
 Joystick.Joystick.Properties.HatCount:=0;
 Joystick.Joystick.Properties.ButtonCount:=0;
 {HID}
 Joystick.Collection:=Collection;

 try
  {Get Report Ids}
  Status:=HIDFindReportIds(Device,Collection,MinId,MaxId);
  if Status <> ERROR_SUCCESS then Exit;

  {Allocate Definitions}
  for Index:=MinId to MaxId do
   begin
    Definition:=HIDAllocateDefinition(Device,Collection,HID_REPORT_INPUT,Index);
    if Definition <> nil then
     begin
      {Check Definition}
      if HIDJoystickCheckDefinition(Definition) = ERROR_SUCCESS then
       begin
        {Link Definition}
        Definition.Next:=Joystick.Definitions;
        Joystick.Definitions:=Definition;
       end
      else
       begin
        {Free Definition}
        HIDFreeDefinition(Definition);
       end;
     end;
   end;

  {Check Definitions}
  if Joystick.Definitions = nil then
   begin
    if HID_LOG_ENABLED then HIDLogError(Device,'Joystick: Failed to allocate definitions');

    Exit;
   end;

  {Allocate Reports}
  Definition:=Joystick.Definitions;
  while Definition <> nil do
   begin
    {Allocate Report}
    Status:=HIDDeviceAllocateReport(Device,Collection,Definition.Id,Definition.Size);
    if Status <> ERROR_SUCCESS then
     begin
      if HID_LOG_ENABLED then HIDLogError(Device,'Joystick: Failed to allocate report id ' + IntToStr(Definition.Id) + ': ' + ErrorToString(Status));

      Exit;
     end;

    {Get Next Definition}
    Definition:=Definition.Next;
   end;

  {Register Joystick}
  Status:=JoystickDeviceRegister(@Joystick.Joystick);
  if Status <> ERROR_SUCCESS then
   begin
    if HID_LOG_ENABLED then HIDLogError(Device,'Joystick: Failed to register new joystick device: ' + ErrorToString(Status));

    Exit;
   end;

  {Update Collection}
  Collection.PrivateData:=Joystick;

  {Start Joystick}
  Status:=JoystickDeviceStart(@Joystick.Joystick);
  if Status <> ERROR_SUCCESS then
   begin
    if HID_LOG_ENABLED then HIDLogError(Device,'Joystick: Failed to start new joystick device: ' + ErrorToString(Status));

    Exit;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  if Result <> ERROR_SUCCESS then
   begin
    {Update Collection}
    Collection.PrivateData:=nil;

    {Deregister Joystick}
    JoystickDeviceDeregister(@Joystick.Joystick);

    {Cancel Reports / Release Reports / Free Definitions}
    Definition:=Joystick.Definitions;
    while Definition <> nil do
     begin
      {Unlink Definition}
      Joystick.Definitions:=Definition.Next;

      {Cancel Report}
      HIDDeviceCancelReport(Device,Definition.Id);

      {Release Report}
      HIDDeviceReleaseReport(Device,Definition.Id);

      {Free Definitions}
      HIDFreeDefinition(Definition);

      {Get Next Definition}
      Definition:=Joystick.Definitions;
     end;

    {Destroy Joystick}
    JoystickDeviceDestroy(@Joystick.Joystick);
   end;
 end;
end;

{==============================================================================}

function HIDJoystickCollectionUnbind(Device:PHIDDevice;Collection:PHIDCollection):LongWord;
{Implementation of HIDCollectionUnbind API for HID Joystick}
{Note: Not intended to be called directly by applications}
var
 Joystick:PHIDJoystickDevice;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Collection}
 if Collection = nil then Exit;

 {$IF DEFINED(JOYSTICK_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Device,'Joystick: Unbinding HID collection (Name=' + DeviceGetName(@Device.Device) + ' Description=' + Device.Device.DeviceDescription + ' Usage=' + IntToHex(Collection.Page,4) + IntToHex(Collection.Usage,4) + ')');
 {$ENDIF}

 {Get Joystick}
 Joystick:=PHIDJoystickDevice(Collection.PrivateData);
 if Joystick = nil then Exit;
 if Joystick.Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Stop Joystick}
 if JoystickDeviceStop(@Joystick.Joystick) <> ERROR_SUCCESS then Exit;

 {Update Collection}
 Collection.PrivateData:=nil;

 {Deregister Joystick}
 if JoystickDeviceDeregister(@Joystick.Joystick) <> ERROR_SUCCESS then Exit;

 {Release Reports / Free Definitions}
 Definition:=Joystick.Definitions;
 while Definition <> nil do
  begin
   {Unlink Definition}
   Joystick.Definitions:=Definition.Next;

   {Release Report}
   HIDDeviceReleaseReport(Device,Definition.Id);

   {Free Definitions}
   HIDFreeDefinition(Definition);

   {Get Next Definition}
   Definition:=Joystick.Definitions;
  end;

 {Destroy Joystick}
 JoystickDeviceDestroy(@Joystick.Joystick);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDJoystickReportReceive(Collection:PHIDCollection;ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord;
{Implementation of HIDReportReceive API for HID Joystick}
{Note: Not intended to be called directly by applications}
var
 Status:LongWord;

 Joystick:PHIDJoystickDevice;
 JoystickData:TJoystickData;

 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Collection}
 if Collection = nil then Exit;

 {$IF DEFINED(JOYSTICK_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Joystick: Report received (Id=' + IntToStr(ReportId) + ' Size=' + IntToStr(ReportSize) + ')');
 {$ENDIF}

 {Get Joystick}
 Joystick:=PHIDJoystickDevice(Collection.PrivateData);
 if Joystick <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Joystick.Joystick.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Joystick.Joystick.ReceiveCount);

      {Check State}
      if Joystick.Joystick.JoystickState <> JOYSTICK_STATE_ENABLED then
       begin
        {$IF DEFINED(JOYSTICK_DEBUG) or DEFINED(HID_DEBUG)}
        if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Joystick: Device disabled, report discarded');
        {$ENDIF}

        Result:=ERROR_SUCCESS;
        Exit;
       end;

      {Get Definition}
      Definition:=Joystick.Definitions;
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
        {Extract Report}
        Status:=HIDJoystickExtractReport(Joystick,Definition,ReportData,ReportSize,@JoystickData);
        if Status = ERROR_SUCCESS then
         begin
          {Check Event}
          if Assigned(Joystick.Joystick.Event) then
           begin
            {Event Parameter}
            JoystickData.Parameter:=Joystick.Joystick.Parameter;

            {Event Callback}
            Joystick.Joystick.Event(@Joystick.Joystick,@JoystickData);
           end
          else
           begin
            {Insert Data}
            JoystickInsertData(@Joystick.Joystick,@JoystickData,True);
           end;
         end
        else
         begin
          if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Joystick: Failed report request, unable to extract report data (Id=' + IntToStr(ReportId) + ' Size=' + IntToStr(ReportSize) + ')');

          {Update Statistics}
          Inc(Joystick.Joystick.ReceiveErrors);
         end;
       end
      else
       begin
        if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Joystick: Failed report request, no report definition or invalid size (Id=' + IntToStr(ReportId) + ' Size=' + IntToStr(ReportSize) + ')');

        {Update Statistics}
        Inc(Joystick.Joystick.ReceiveErrors);
       end;

      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Joystick.Joystick.Lock);
     end;
    end
   else
    begin
     if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Joystick: Failed to acquire lock');
    end;
  end
 else
  begin
   if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Joystick: Report request invalid');
  end;
end;

{==============================================================================}

function HIDJoystickGetCounts(Joystick:PHIDJoystickDevice;var HatCount,AxisCount,ButtonCount:LongWord):LongWord;
{Scan the allocated HID input definitions to determine the Hat, Axis and Button counts}
{Note: Not intended to be called directly by applications}
var
 Field:PHIDField;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Defaults}
 HatCount:=0;
 AxisCount:=0;
 ButtonCount:=0;

 {Check Joystick}
 if Joystick = nil then Exit;

 {Check Definitions}
 Definition:=Joystick.Definitions;
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
         {X Axis}
         HID_DESKTOP_X:Inc(AxisCount);
         {Y Axis}
         HID_DESKTOP_Y:Inc(AxisCount);
         {Z Axis}
         HID_DESKTOP_Z:Inc(AxisCount);
         {Rotation X Axis}
         HID_DESKTOP_RX:Inc(AxisCount);
         {Rotation Y Axis}
         HID_DESKTOP_RY:Inc(AxisCount);
         {Rotation Z Axis}
         HID_DESKTOP_RZ:Inc(AxisCount);
         {Slider Control (Axis)}
         HID_DESKTOP_SLIDER:Inc(AxisCount);
         {Hat Switch}
         HID_DESKTOP_HAT_SWITCH:Inc(HatCount);
        end;
       end;
      HID_PAGE_BUTTON:begin
        {Check Usage}
        case Field.Usage of
         {Buttons}
         HID_BUTTON_PRIMARY..HID_BUTTON_65535:Inc(ButtonCount);
        end;
       end;
     end;

     {Get Next Field}
     Field:=Field.Next;
    end;

   {Get Next Definition}
   Definition:=Definition.Next;
  end;

 {Check Counts}
 if HatCount > JOYSTICK_MAX_HAT then HatCount:=JOYSTICK_MAX_HAT;
 if AxisCount > JOYSTICK_MAX_AXIS then AxisCount:=JOYSTICK_MAX_AXIS;
 if ButtonCount > JOYSTICK_MAX_BUTTON then ButtonCount:=JOYSTICK_MAX_BUTTON;

 {$IF DEFINED(JOYSTICK_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Joystick.Collection.Device,'Joystick: Hat Count=' + IntToStr(HatCount) + ' Axis Count=' + IntToStr(AxisCount) + ' Button Count=' + IntToStr(ButtonCount));
 {$ENDIF}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDJoystickGetControls(Joystick:PHIDJoystickDevice;HatCount,AxisCount,ButtonCount:LongWord):LongWord;
{Scan the allocated HID input definitions to configure the Hat, Axis and Button controls}
{Note: Not intended to be called directly by applications}

 procedure HIDUpdateHat(Name:LongWord;Hat:PJoystickHat;Field:PHIDField);
 begin
  {}
  {Check Hat}
  if Hat = nil then Exit;

  {Check Field}
  if Field = nil then Exit;

  {Update Hat}
  Hat.Name:=Name;
  Hat.Logical.Minimum:=Field.Logical.Minimum;
  Hat.Logical.Maximum:=Field.Logical.Maximum;
  Hat.Physical.Minimum:=Field.Physical.Minimum;
  Hat.Physical.Maximum:=Field.Physical.Maximum;
  Hat.Multiplier:=Field.Multiplier;
  Hat.Resolution:=Field.Resolution;
 end;

 procedure HIDUpdateAxis(Name:LongWord;Axis:PJoystickAxis;Field:PHIDField);
 begin
  {}
  {Check Axis}
  if Axis = nil then Exit;

  {Check Field}
  if Field = nil then Exit;

  {Update Axis}
  Axis.Name:=Name;
  Axis.Logical.Minimum:=Field.Logical.Minimum;
  Axis.Logical.Maximum:=Field.Logical.Maximum;
  Axis.Physical.Minimum:=Field.Physical.Minimum;
  Axis.Physical.Maximum:=Field.Physical.Maximum;
  Axis.Multiplier:=Field.Multiplier;
  Axis.Resolution:=Field.Resolution;
 end;

var
 Count:LongWord;

 IndexHat:LongWord;
 IndexAxis:LongWord;
 IndexButton:LongWord;

 Field:PHIDField;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;

 {Update Counts}
 Joystick.Joystick.Properties.HatCount:=HatCount;
 Joystick.Joystick.Properties.AxisCount:=AxisCount;
 Joystick.Joystick.Properties.ButtonCount:=ButtonCount;

 {Set Defaults}
 IndexHat:=0;
 IndexAxis:=0;
 IndexButton:=0;

 {Check Definitions}
 Definition:=Joystick.Definitions;
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
           {X Axis}
           if IndexAxis < JOYSTICK_MAX_AXIS then
            begin
             HIDUpdateAxis(JOYSTICK_AXIS_X,@Joystick.Joystick.Properties.Axes[IndexAxis],Field);

             Inc(IndexAxis);
            end;
          end;
         HID_DESKTOP_Y:begin
           {Y Axis}
           if IndexAxis < JOYSTICK_MAX_AXIS then
            begin
             HIDUpdateAxis(JOYSTICK_AXIS_Y,@Joystick.Joystick.Properties.Axes[IndexAxis],Field);

             Inc(IndexAxis);
            end;
          end;
         HID_DESKTOP_Z:begin
           {Z Axis}
           if IndexAxis < JOYSTICK_MAX_AXIS then
            begin
             HIDUpdateAxis(JOYSTICK_AXIS_Z,@Joystick.Joystick.Properties.Axes[IndexAxis],Field);

             Inc(IndexAxis);
            end;
          end;
         HID_DESKTOP_RX:begin
           {Rotation X Axis}
           if IndexAxis < JOYSTICK_MAX_AXIS then
            begin
             HIDUpdateAxis(JOYSTICK_ROTATION_X,@Joystick.Joystick.Properties.Axes[IndexAxis],Field);

             Inc(IndexAxis);
            end;
          end;
         HID_DESKTOP_RY:begin
           {Rotation Y Axis}
           if IndexAxis < JOYSTICK_MAX_AXIS then
            begin
             HIDUpdateAxis(JOYSTICK_ROTATION_Y,@Joystick.Joystick.Properties.Axes[IndexAxis],Field);

             Inc(IndexAxis);
            end;
          end;
         HID_DESKTOP_RZ:begin
           {Rotation Z Axis}
           if IndexAxis < JOYSTICK_MAX_AXIS then
            begin
             HIDUpdateAxis(JOYSTICK_ROTATION_Z,@Joystick.Joystick.Properties.Axes[IndexAxis],Field);

             Inc(IndexAxis);
            end;
          end;
         HID_DESKTOP_SLIDER:begin
           {Slider Control (Axis)}
           if IndexAxis < JOYSTICK_MAX_AXIS then
            begin
             HIDUpdateAxis(JOYSTICK_SLIDER,@Joystick.Joystick.Properties.Axes[IndexAxis],Field);

             Inc(IndexAxis);
            end;
          end;
         HID_DESKTOP_HAT_SWITCH:begin
           {Hat Switch}
           if IndexHat < JOYSTICK_MAX_HAT then
            begin
             HIDUpdateHat(JOYSTICK_HAT_POV,@Joystick.Joystick.Properties.Hats[IndexHat],Field);

             Inc(IndexHat);
            end;
          end;
        end;
       end;
      HID_PAGE_BUTTON:begin
        {Check Usage}
        case Field.Usage of
         HID_BUTTON_PRIMARY..HID_BUTTON_65535:begin
           {Buttons}
           if IndexButton < JOYSTICK_MAX_BUTTON then
            begin
             Joystick.Joystick.Properties.Buttons[IndexButton]:=IndexButton + 1;

             Inc(IndexButton);
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

 {Clear Unassigned Hats}
 if HatCount < JOYSTICK_MAX_HAT then
  begin
   for Count:=JOYSTICK_MAX_HAT - 1 downto HatCount do
    begin
     FillChar(Joystick.Joystick.Properties.Hats[Count],SizeOf(TJoystickHat),0);
    end;
  end;

 {Clear Unassigned Axes}
 if AxisCount < JOYSTICK_MAX_AXIS then
  begin
   for Count:=JOYSTICK_MAX_AXIS - 1 downto AxisCount do
    begin
     FillChar(Joystick.Joystick.Properties.Axes[Count],SizeOf(TJoystickAxis),0);
    end;
  end;

 {Clear Unassigned Buttons}
 if ButtonCount < JOYSTICK_MAX_BUTTON then
  begin
   for Count:=JOYSTICK_MAX_BUTTON - 1 downto ButtonCount do
    begin
     Joystick.Joystick.Properties.Buttons[Count]:=JOYSTICK_BUTTON_NONE;
    end;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDJoystickExtractReport(Joystick:PHIDJoystickDevice;Definition:PHIDDefinition;Buffer:Pointer;Size:LongWord;Data:PJoystickData):LongWord;
{Process the received HID report against the matching HID definition and extract all available fields}
{Note: Not intended to be called directly by applications}
var
 Value:Boolean;
 Offset:LongInt;

 Field:PHIDField;

 Index:LongInt;
 IndexHat:LongWord;
 IndexAxis:LongWord;
 IndexButton:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;

 {Check Definition}
 if Definition = nil then Exit;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Size}
 if Size < Definition.Size then Exit;

 {Check Data}
 if Data = nil then Exit;

 {Set Defaults}
 IndexHat:=0;
 IndexAxis:=0;
 IndexButton:=0;

 {Clear Data}
 FillChar(Data^,SizeOf(TJoystickData),0);

 {Get Counts}
 Data.AxisCount:=Joystick.Joystick.Properties.AxisCount;
 Data.HatCount:=Joystick.Joystick.Properties.HatCount;
 Data.ButtonCount:=Joystick.Joystick.Properties.ButtonCount;

 {Extract Fields}
 Field:=Definition.Fields;
 while Field <> nil do
  begin
   {Check Page}
   case Field.Page of
    HID_PAGE_GENERIC_DESKTOP:begin
      {Check Usage}
      case Field.Usage of
       HID_DESKTOP_X,
       HID_DESKTOP_Y,
       HID_DESKTOP_Z,
       HID_DESKTOP_RX,
       HID_DESKTOP_RY,
       HID_DESKTOP_RZ,
       HID_DESKTOP_SLIDER:begin
         {X, Y, Z, Rotation X, Y, Z Axes and Slider Controls}
         if IndexAxis < JOYSTICK_MAX_AXIS then
          begin
           if Joystick.Joystick.Properties.Axes[IndexAxis].Logical.Minimum < 0 then
            begin
             HIDExtractSignedField(Field,Buffer,Size,Offset);
            end
           else
            begin
             HIDExtractUnsignedField(Field,Buffer,Size,LongWord(Offset));
            end;

           Data.Axes[IndexAxis]:=Offset;

           Inc(IndexAxis);
          end;
        end;
       HID_DESKTOP_HAT_SWITCH:begin
         {Hat Switch}
         if IndexHat < JOYSTICK_MAX_HAT then
          begin
           if Joystick.Joystick.Properties.Hats[IndexHat].Logical.Minimum < 0 then
            begin
             HIDExtractSignedField(Field,Buffer,Size,Offset);
            end
           else
            begin
             HIDExtractUnsignedField(Field,Buffer,Size,LongWord(Offset));
            end;

           Data.Hats[IndexHat]:=Offset;

           Inc(IndexHat);
          end;
        end;
      end;
     end;
    HID_PAGE_BUTTON:begin
      {Check Usage}
      case Field.Usage of
       HID_BUTTON_PRIMARY..HID_BUTTON_65535:begin
         {Buttons}
         if IndexButton < JOYSTICK_MAX_BUTTON then
          begin
           HIDExtractBitField(Field,Buffer,Size,Value);

           if Value then Data.Buttons:=Data.Buttons or (1 shl IndexButton);

           Inc(IndexButton);
          end;
        end;
      end;
     end;
   end;

   {Get Next Field}
   Field:=Field.Next;
  end;

 {$IF DEFINED(JOYSTICK_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then
  begin
   HIDLogDebug(Joystick.Collection.Device,'Joystick: Report data (Axis Count=' + IntToStr(Data.AxisCount) + ' Hat Count=' + IntToStr(Data.HatCount) + ' Button Count=' + IntToStr(Data.ButtonCount) + ')');
   for Index:=0 to Data.AxisCount - 1 do
    begin
     HIDLogDebug(Joystick.Collection.Device,'                      (Axis ' + IntToStr(Index) + '=' + IntToStr(Data.Axes[Index]) + ')');
    end;
   for Index:=0 to Data.HatCount - 1 do
    begin
     HIDLogDebug(Joystick.Collection.Device,'                      (Hat ' + IntToStr(Index) + '=' + IntToStr(Data.Hats[Index]) + ')');
    end;
   for Index:=0 to Data.ButtonCount - 1 do
    begin
     HIDLogDebug(Joystick.Collection.Device,'                      (Button ' + IntToStr(Index) + '=' + BoolToStr((Data.Buttons and (1 shl Index)) <> 0,True) + ')');
    end;
  end;
 {$ENDIF}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}

initialization
 HIDJoystickInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

