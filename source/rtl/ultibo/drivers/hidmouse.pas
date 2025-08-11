{
Ultibo HID Mouse consumer unit.

Copyright (C) 2023 - SoftOz Pty Ltd.

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


HID Mouse
=========

 This is a consumer for any generic HID mouse device, it accepts HID application
 collections in the generic desktop page (HID_PAGE_GENERIC_DESKTOP) with the usage
 set to mouse (HID_DESKTOP_MOUSE).

 The consumer will bind to any mouse collection that implements at a minimum the
 X and Y axis and the primary button. However the data reported can include X, Y
 and wheel as well as up to 5 buttons including left, right and middle.

 A mouse can report either absolute or relative positioning, an absolute position
 for X, Y or wheel will be reported in the buttons field of the mouse data packet
 using the MOUSE_ABSOLUTE_* flags.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit HIDMouse;
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
  Core.HID,
  Core.Mouse,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  HID,
  Mouse,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {HID Mouse specific constants}
 HID_MOUSE_CONSUMER_NAME = 'HID Mouse Consumer'; {Name of HID Mouse consumer}

 HID_MOUSE_DESCRIPTION = 'HID Mouse'; {Description of HID Mouse device}

{==============================================================================}
type
 {HID Mouse specific types}
 PHIDMouseDevice = ^THIDMouseDevice;
 THIDMouseDevice = record
  {Mouse Properties}
  Mouse:TMouseDevice;
  {HID Properties}
  Collection:PHIDCollection;   {The HID collection this mouse is bound to}
  Definitions:PHIDDefinition;  {The input report definitions that can be accepted as mouse reports}
 end;

{==============================================================================}
{var}
 {HID Mouse specific variables}

{==============================================================================}
{Initialization Functions}
procedure HIDMouseInit;

{==============================================================================}
{HID Mouse Functions}

{==============================================================================}
{HID Mouse Helper Functions}
function HIDMouseCheckCollection(Collection:PHIDCollection):LongWord;
function HIDMouseCheckDefinition(Definition:PHIDDefinition):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {HID Mouse specific variables}
 HIDMouseInitialized:Boolean;

 HIDMouseConsumer:PHIDConsumer; {HID Consumer interface (Set by HIDMouseInit)}

{==============================================================================}
{==============================================================================}
{Forward Declarations}
function HIDMouseCollectionBind(Device:PHIDDevice;Collection:PHIDCollection):LongWord; forward;
function HIDMouseCollectionUnbind(Device:PHIDDevice;Collection:PHIDCollection):LongWord; forward;

function HIDMouseReportReceive(Collection:PHIDCollection;ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure HIDMouseInit;
{Initialize the HID Mouse unit and HID Mouse driver}

{Note: Called only during system startup}
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if HIDMouseInitialized then Exit;

 if HID_REGISTER_MOUSE then
  begin
   {Create HID Mouse Consumer}
   HIDMouseConsumer:=HIDConsumerCreate;
   if HIDMouseConsumer <> nil then
    begin
     {Update HID Mouse Consumer}
     {Driver}
     HIDMouseConsumer.Driver.DriverName:=HID_MOUSE_CONSUMER_NAME;
     {HID}
     HIDMouseConsumer.CollectionBind:=HIDMouseCollectionBind;
     HIDMouseConsumer.CollectionUnbind:=HIDMouseCollectionUnbind;
     HIDMouseConsumer.ReportReceive:=HIDMouseReportReceive;

     {Register HID Mouse Consumer}
     Status:=HIDConsumerRegister(HIDMouseConsumer);
     if Status <> ERROR_SUCCESS then
      begin
       if HID_LOG_ENABLED then HIDLogError(nil,'Mouse: Failed to register HID Mouse consumer: ' + ErrorToString(Status));

       {Destroy Consumer}
       HIDConsumerDestroy(HIDMouseConsumer);

       HIDMouseConsumer:=nil;
      end;
    end
   else
    begin
     if HID_LOG_ENABLED then HIDLogError(nil,'Mouse: Failed to create HID Mouse consumer');
    end;
  end;

 HIDMouseInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{HID Mouse Functions}

{==============================================================================}
{==============================================================================}
{HID Mouse Helper Functions}
function HIDMouseCheckCollection(Collection:PHIDCollection):LongWord;
{Check if a HID collection is suitable for use as a mouse device}
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
     HID_DESKTOP_MOUSE:begin
       {Check Flags}
       if Collection.Flags = HID_MAIN_COLLECTION_APPLICATION then
        begin
         if HID_MOUSE_REJECT_TOUCH then
          begin
           {Check Touch Collection}
           if HIDFindCollection(Collection.Device,HID_PAGE_DIGITIZERS,HID_DIGITIZERS_TOUCH_SCREEN) <> nil then Exit;
          end;

         Result:=ERROR_SUCCESS;
        end;
      end;
    end;
   end;
 end;
end;

{==============================================================================}

function HIDMouseCheckDefinition(Definition:PHIDDefinition):LongWord;
{Check if a HID definition is suitable for use as a mouse input report}
var
 HasX:Boolean;
 HasY:Boolean;
 HasPrimary:Boolean;
 {HasSecondary:Boolean;}
 Field:PHIDField;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Definition}
 if Definition = nil then Exit;

 {Set Defaults}
 HasX:=False;
 HasY:=False;
 HasPrimary:=False;
 {HasSecondary:=False;}

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
      {Check Primary}
      if (Field.Usage >= HID_BUTTON_PRIMARY) and (Field.Usage + Field.Count - 1 <= HID_BUTTON_PRIMARY) then
       begin
        HasPrimary:=True;
       end;

      {Check Secondary}
      {if (Field.Usage >= HID_BUTTON_SECONDARY) and (Field.Usage + Field.Count - 1 <= HID_BUTTON_SECONDARY) then
       begin
        HasSecondary:=True;
       end;}
     end;
   end;

   {Get Next Field}
   Field:=Field.Next;
  end;

 {Check Result}
 if HasX and HasY and HasPrimary {and HasSecondary} then Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{HID Mouse Internal Functions}
function HIDMouseCollectionBind(Device:PHIDDevice;Collection:PHIDCollection):LongWord;
{Implementation of HIDCollectionBind API for HID Mouse}
{Note: Not intended to be called directly by applications}
var
 MinId:Byte;
 MaxId:Byte;
 Index:LongWord;
 Status:LongWord;
 Mouse:PHIDMouseDevice;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Collection}
 if Collection = nil then Exit;

 {$IF DEFINED(MOUSE_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Device,'Mouse: Attempting to bind HID collection (Name=' + DeviceGetName(@Device.Device) + ' Description=' + Device.Device.DeviceDescription + ' Usage=' + IntToHex(Collection.Page,4) + IntToHex(Collection.Usage,4) + ')');
 {$ENDIF}

 {Set Result}
 Result:=ERROR_NOT_SUPPORTED;

 {Check Collection}
 Status:=HIDMouseCheckCollection(Collection);
 if Status <> ERROR_SUCCESS then Exit;

 {Create Mouse}
 Mouse:=PHIDMouseDevice(MouseDeviceCreateEx(SizeOf(THIDMouseDevice)));
 if Mouse = nil then
  begin
   if HID_LOG_ENABLED then HIDLogError(Device,'Mouse: Failed to create new mouse device');

   Exit;
  end;

 {Update Mouse}
 {Device}
 Mouse.Mouse.Device.DeviceBus:=DEVICE_BUS_NONE;
 Mouse.Mouse.Device.DeviceType:=MOUSE_TYPE_NONE;
 if Device.Device.DeviceType = HID_TYPE_USB then
  begin
   Mouse.Mouse.Device.DeviceBus:=DEVICE_BUS_USB;
   Mouse.Mouse.Device.DeviceType:=MOUSE_TYPE_USB;
  end;
 Mouse.Mouse.Device.DeviceFlags:=Mouse.Mouse.Device.DeviceFlags; {Don't override defaults (was MOUSE_FLAG_NONE)}
 Mouse.Mouse.Device.DeviceData:=Device;
 Mouse.Mouse.Device.DeviceDescription:=Device.Device.DeviceDescription; {HID_MOUSE_DESCRIPTION}
 {Mouse}
 Mouse.Mouse.MouseState:=MOUSE_STATE_ATTACHING;
 {Driver}
 Mouse.Mouse.Properties.MaxButtons:=MOUSE_LEFT_BUTTON or MOUSE_RIGHT_BUTTON or MOUSE_MIDDLE_BUTTON;
 {HID}
 Mouse.Collection:=Collection;

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
      if HIDMouseCheckDefinition(Definition) = ERROR_SUCCESS then
       begin
        {Link Definition}
        Definition.Next:=Mouse.Definitions;
        Mouse.Definitions:=Definition;
       end
      else
       begin
        {Free Definition}
        HIDFreeDefinition(Definition);
       end;
     end;
   end;

  {Check Definitions}
  if Mouse.Definitions = nil then
   begin
    if HID_LOG_ENABLED then HIDLogError(Device,'Mouse: Failed to allocate definitions');

    Exit;
   end;

  {Allocate Reports}
  Definition:=Mouse.Definitions;
  while Definition <> nil do
   begin
    {Allocate Report}
    Status:=HIDDeviceAllocateReport(Device,Collection,Definition.Id,Definition.Size);
    if Status <> ERROR_SUCCESS then
     begin
      if HID_LOG_ENABLED then HIDLogError(Device,'Mouse: Failed to allocate report id ' + IntToStr(Definition.Id) + ': ' + ErrorToString(Status));

      Exit;
     end;

    {Get Next Definition}
    Definition:=Definition.Next;
   end;

  {Register Mouse}
  Status:=MouseDeviceRegister(@Mouse.Mouse);
  if Status <> ERROR_SUCCESS then
   begin
    if HID_LOG_ENABLED then HIDLogError(Device,'Mouse: Failed to register new mouse device: ' + ErrorToString(Status));

    Exit;
   end;

  {Check Interval}
  if USB_MOUSE_POLLING_INTERVAL > 0 then
   begin
    {Set Interval}
    HIDDeviceSetInterval(Device,USB_MOUSE_POLLING_INTERVAL);
   end;

  {Update Collection}
  Collection.PrivateData:=Mouse;

  {Submit Reports}
  Definition:=Mouse.Definitions;
  while Definition <> nil do
   begin
    Status:=HIDDeviceSubmitReport(Device,Definition.Id);
    if Status <> ERROR_SUCCESS then
     begin
      if HID_LOG_ENABLED then HIDLogError(Device,'Mouse: Failed to submit report id ' + IntToStr(Definition.Id) + ': ' + ErrorToString(Status));

      Exit;
     end;

    {Get Next Definition}
    Definition:=Definition.Next;
   end;

  {Set State to Attached}
  if MouseDeviceSetState(@Mouse.Mouse,MOUSE_STATE_ATTACHED) <> ERROR_SUCCESS then Exit;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  if Result <> ERROR_SUCCESS then
   begin
    {Update Collection}
    Collection.PrivateData:=nil;

    {Deregister Mouse}
    MouseDeviceDeregister(@Mouse.Mouse);

    {Cancel Reports / Release Reports / Free Definitions}
    Definition:=Mouse.Definitions;
    while Definition <> nil do
     begin
      {Unlink Definition}
      Mouse.Definitions:=Definition.Next;

      {Cancel Report}
      HIDDeviceCancelReport(Device,Definition.Id);

      {Release Report}
      HIDDeviceReleaseReport(Device,Definition.Id);

      {Free Definitions}
      HIDFreeDefinition(Definition);

      {Get Next Definition}
      Definition:=Mouse.Definitions;
     end;

    {Destroy Mouse}
    MouseDeviceDestroy(@Mouse.Mouse);
   end;
 end;
end;

{==============================================================================}

function HIDMouseCollectionUnbind(Device:PHIDDevice;Collection:PHIDCollection):LongWord;
{Implementation of HIDCollectionUnbind API for HID Mouse}
{Note: Not intended to be called directly by applications}
var
 Mouse:PHIDMouseDevice;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Collection}
 if Collection = nil then Exit;

 {$IF DEFINED(MOUSE_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Device,'Mouse: Unbinding HID collection (Name=' + DeviceGetName(@Device.Device) + ' Description=' + Device.Device.DeviceDescription + ' Usage=' + IntToHex(Collection.Page,4) + IntToHex(Collection.Usage,4) + ')');
 {$ENDIF}

 {Get Mouse}
 Mouse:=PHIDMouseDevice(Collection.PrivateData);
 if Mouse = nil then Exit;
 if Mouse.Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Set State to Detaching}
 Result:=ERROR_OPERATION_FAILED;
 if MouseDeviceSetState(@Mouse.Mouse,MOUSE_STATE_DETACHING) <> ERROR_SUCCESS then Exit;

 {Acquire the Lock}
 if MutexLock(Mouse.Mouse.Lock) <> ERROR_SUCCESS then Exit;

 {Cancel Reports}
 Definition:=Mouse.Definitions;
 while Definition <> nil do
  begin
   {Cancel Report}
   HIDDeviceCancelReport(Device,Definition.Id);

   {Get Next Definition}
   Definition:=Definition.Next;
  end;

 {Release the Lock}
 MutexUnlock(Mouse.Mouse.Lock);

 {Set State to Detached}
 if MouseDeviceSetState(@Mouse.Mouse,MOUSE_STATE_DETACHED) <> ERROR_SUCCESS then Exit;

 {Update Collection}
 Collection.PrivateData:=nil;

 {Deregister Mouse}
 if MouseDeviceDeregister(@Mouse.Mouse) <> ERROR_SUCCESS then Exit;

 {Release Reports / Free Definitions}
 Definition:=Mouse.Definitions;
 while Definition <> nil do
  begin
   {Unlink Definition}
   Mouse.Definitions:=Definition.Next;

   {Release Report}
   HIDDeviceReleaseReport(Device,Definition.Id);

   {Free Definitions}
   HIDFreeDefinition(Definition);

   {Get Next Definition}
   Definition:=Mouse.Definitions;
  end;

 {Destroy Mouse}
 MouseDeviceDestroy(@Mouse.Mouse);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDMouseReportReceive(Collection:PHIDCollection;ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord;
{Implementation of HIDReportReceive API for HID Mouse}
{Note: Not intended to be called directly by applications}
var
 Buttons:Word;
 Value:Boolean;
 OffsetX:LongInt;
 OffsetY:LongInt;
 OffsetTemp:LongInt;
 OffsetWheel:LongInt;
 MaximumX:LongWord;
 MaximumY:LongWord;
 MaximumTemp:LongWord;
 MaximumWheel:LongWord;

 Mouse:PHIDMouseDevice;
 MouseData:TMouseData;

 Field:PHIDField;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Collection}
 if Collection = nil then Exit;

 {$IF DEFINED(MOUSE_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Mouse: Report received (Id=' + IntToStr(ReportId) + ' Size=' + IntToStr(ReportSize) + ')');
 {$ENDIF}

 {Get Mouse}
 Mouse:=PHIDMouseDevice(Collection.PrivateData);
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
        {$IF DEFINED(MOUSE_DEBUG) or DEFINED(HID_DEBUG)}
        if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Mouse: Detachment pending, report discarded');
        {$ENDIF}

        Result:=ERROR_SUCCESS;
        Exit;
       end;

      {Get Definition}
      Definition:=Mouse.Definitions;
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
        Value:=False;
        Buttons:=0;
        OffsetX:=0;
        OffsetY:=0;
        OffsetWheel:=0;
        MaximumX:=0;
        MaximumY:=0;
        MaximumWheel:=0;

        {Clear Mouse Data}
        FillChar(MouseData,SizeOf(TMouseData),0);

        {Extract Fields}
        Field:=Definition.Fields;
        while Field <> nil do
         begin
          {Check Page}
          case Field.Page of
           HID_PAGE_BUTTON:begin
             {Check Usage}
             case Field.Usage of
              {Buttons}
              HID_BUTTON_PRIMARY:begin
                HIDExtractBitField(Field,ReportData,ReportSize,Value);

                {Check Flags}
                if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
                 begin
                  if Value then Buttons:=Buttons or MOUSE_LEFT_BUTTON;
                 end
                else
                 begin
                  if Value then Buttons:=Buttons or MOUSE_RIGHT_BUTTON;
                 end;
               end;
              HID_BUTTON_SECONDARY:begin
                HIDExtractBitField(Field,ReportData,ReportSize,Value);

                {Check Flags}
                if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
                 begin
                  if Value then Buttons:=Buttons or MOUSE_RIGHT_BUTTON;
                 end
                else
                 begin
                  if Value then Buttons:=Buttons or MOUSE_LEFT_BUTTON;
                 end;
               end;
              HID_BUTTON_TERTIARY:begin
                HIDExtractBitField(Field,ReportData,ReportSize,Value);

                if Value then Buttons:=Buttons or MOUSE_MIDDLE_BUTTON;
               end;
              HID_BUTTON_4:begin
                HIDExtractBitField(Field,ReportData,ReportSize,Value);

                if Value then Buttons:=Buttons or MOUSE_SIDE_BUTTON;
               end;
              HID_BUTTON_5:begin
                HIDExtractBitField(Field,ReportData,ReportSize,Value);

                if Value then Buttons:=Buttons or MOUSE_EXTRA_BUTTON;
               end;
             end;
            end;
           HID_PAGE_GENERIC_DESKTOP:begin
             {Check Usage}
             case Field.Usage of
              {X, Y and Wheel}
              HID_DESKTOP_X:begin
                HIDExtractSignedField(Field,ReportData,ReportSize,OffsetX);

                {Check Absolute}
                if (Field.Flags and HID_MAIN_ITEM_RELATIVE) = 0 then
                 begin
                  Buttons:=Buttons or MOUSE_ABSOLUTE_X;

                  MaximumX:=Field.Logical.Maximum;
                 end;
               end;
              HID_DESKTOP_Y:begin
                HIDExtractSignedField(Field,ReportData,ReportSize,OffsetY);

                {Check Absolute}
                if (Field.Flags and HID_MAIN_ITEM_RELATIVE) = 0 then
                 begin
                  Buttons:=Buttons or MOUSE_ABSOLUTE_Y;

                  MaximumY:=Field.Logical.Maximum;
                 end;
               end;
              HID_DESKTOP_WHEEL:begin
                HIDExtractSignedField(Field,ReportData,ReportSize,OffsetWheel);

                {Check Absolute}
                if (Field.Flags and HID_MAIN_ITEM_RELATIVE) = 0 then
                 begin
                  Buttons:=Buttons or MOUSE_ABSOLUTE_WHEEL;

                  MaximumWheel:=Field.Logical.Maximum;
                 end;
               end;
             end;
            end;
          end;

          {Get Next Field}
          Field:=Field.Next;
         end;

        {$IF DEFINED(MOUSE_DEBUG) or DEFINED(HID_DEBUG)}
        if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Mouse: Report data (X=' + IntToStr(OffsetX) + ' Y=' + IntToStr(OffsetY) + ' Wheel=' + IntToStr(OffsetWheel) + ' Buttons=' + IntToHex(Buttons,8) + ')');
        {$ENDIF}

        {Get Buttons}
        MouseData.Buttons:=Buttons;

        {Check Swap}
        if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_XY) <> 0 then
         begin
          {Swap Offset X/Y}
          OffsetTemp:=OffsetX;
          OffsetX:=OffsetY;
          OffsetY:=OffsetTemp;
         end;

        {Check Invert}
        if (Mouse.Mouse.Device.DeviceFlags and  MOUSE_FLAG_INVERT_X) <> 0 then
         begin
          {Invert Offset X}
          OffsetX:=-OffsetX;
         end;
        if (Mouse.Mouse.Device.DeviceFlags and  MOUSE_FLAG_INVERT_Y) <> 0 then
         begin
          {Invert Offset Y}
          OffsetY:=-OffsetY;
         end;

        {Check Swap Max}
        if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_MAX_XY) <> 0 then
         begin
          {Swap Maximum X/Y}
          MaximumTemp:=MaximumX;
          MaximumX:=MaximumY;
          MaximumY:=MaximumTemp;
         end;

        {Check Rotation}
        case Mouse.Mouse.Properties.Rotation of
         MOUSE_ROTATION_0:begin
           {Get Offset X and Y}
           MouseData.OffsetX:=OffsetX;
           MouseData.OffsetY:=OffsetY;

           {Get Maximum X, Y}
           MouseData.MaximumX:=MaximumX;
           MouseData.MaximumY:=MaximumY;
          end;
         MOUSE_ROTATION_90:begin
           {Swap Offset X and Y, Invert Offset X}
           MouseData.OffsetX:=-OffsetY;
           MouseData.OffsetY:=OffsetX;

           {Get Maximum X, Y}
           MouseData.MaximumX:=MaximumY;
           MouseData.MaximumY:=MaximumX;
          end;
         MOUSE_ROTATION_180:begin
           {Invert Offset X and Y}
           MouseData.OffsetX:=-OffsetX;
           MouseData.OffsetY:=-OffsetY;

           {Get Maximum X, Y}
           MouseData.MaximumX:=MaximumX;
           MouseData.MaximumY:=MaximumY;
          end;
         MOUSE_ROTATION_270:begin
           {Swap Offset X and Y, Invert Offset Y}
           MouseData.OffsetX:=OffsetY;
           MouseData.OffsetY:=-OffsetX;

           {Get Maximum X, Y}
           MouseData.MaximumX:=MaximumY;
           MouseData.MaximumY:=MaximumX;
          end;
        end;
        {Get Wheel Offset and Maximum}
        MouseData.OffsetWheel:=OffsetWheel;
        MouseData.MaximumWheel:=MaximumWheel;

        {Insert Data}
        MouseInsertData(@Mouse.Mouse,@MouseData,True);
       end
      else
       begin
        if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Mouse: Failed report request, no report definition or invalid size (Id=' + IntToStr(ReportId) + ' Size=' + IntToStr(ReportSize) + ')');

        {Update Statistics}
        Inc(Mouse.Mouse.ReceiveErrors);
       end;

      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Mouse.Mouse.Lock);
     end;
    end
   else
    begin
     if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Mouse: Failed to acquire lock');
    end;
  end
 else
  begin
   if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Mouse: Report request invalid');
  end;
end;

{==============================================================================}
{==============================================================================}

initialization
 HIDMouseInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
