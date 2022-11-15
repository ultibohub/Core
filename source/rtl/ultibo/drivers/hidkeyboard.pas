{
Ultibo HID Keyboard consumer unit.

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


HID Keyboard
============

 This is a consumer for any generic HID keyboard device, it accepts HID application
 collections in the generic desktop page (HID_PAGE_GENERIC_DESKTOP) with the usage
 set to keyboard (HID_DESKTOP_KEYBOARD).

 The consumer will bind to any keyboard collection that includes at a minimum an input
 report containing a field that provides keypress data from the keyboard / keypad page
 (HID_PAGE_KEYBOARD_KEYPAD) and a field providing modifier keys from the same page.

 The consumer will also look for an output report containing a field from the LED page
 (HID_PAGE_LED) and will use this to set the keyboard LEDs if found.

 Up to 16 keypresses per report can be accepted (see HID_KEYBOARD_MAX_KEYS) but most
 keyboards will commonly report up to 6 which is the size defined in the USB HID usage
 tables for boot mode keyboard reports.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit HIDKeyboard;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,HID,Keyboard,Keymap,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {HID Keyboard specific constants}
 HID_KEYBOARD_CONSUMER_NAME = 'HID Keyboard Consumer'; {Name of HID Keyboard consumer}

 HID_KEYBOARD_DESCRIPTION = 'HID Keyboard'; {Description of HID Keyboard device}

 HID_KEYBOARD_MAX_KEYS = 16; {Maximum number of keys allowed in any report}

{==============================================================================}
type
 {HID Keyboard specific types}
 {HID Keyboard Keys}
 PHIDKeyboardKeys = ^THIDKeyboardKeys;
 THIDKeyboardKeys = array[0..HID_KEYBOARD_MAX_KEYS - 1] of Byte;

 {HID Keyboard Device}
 PHIDKeyboardDevice = ^THIDKeyboardDevice;
 THIDKeyboardDevice = record
  {Keyboard Properties}
  Keyboard:TKeyboardDevice;
  {HID Properties}
  Collection:PHIDCollection;    {The HID collection this keyboard is bound to}
  Definitions:PHIDDefinition;   {The input report definitions that can be accepted as keyboard reports}
  LEDDefinition:PHIDDefinition; {The report definition that corresponds to the LED output}
  LastCode:Word;                {The scan code of the last key pressed}
  LastCount:LongWord;           {The repeat count of the last key pressed}
  LastKeys:THIDKeyboardKeys;    {The keys from the last keyboard report received}
 end;

{==============================================================================}
{var}
 {HID Keyboard specific variables}

{==============================================================================}
{Initialization Functions}
procedure HIDKeyboardInit;

{==============================================================================}
{HID Keyboard Functions}
function HIDKeyboardDeviceControl(Keyboard:PKeyboardDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

{==============================================================================}
{HID Keyboard Helper Functions}
function HIDKeyboardCheckCollection(Collection:PHIDCollection):LongWord;
function HIDKeyboardCheckInputDefinition(Definition:PHIDDefinition):LongWord;
function HIDKeyboardCheckOutputDefinition(Definition:PHIDDefinition):LongWord;

function HIDKeyboardCheckPressed(Keyboard:PHIDKeyboardDevice;ScanCode:Byte):Boolean;
function HIDKeyboardCheckRepeated(Keyboard:PHIDKeyboardDevice;ScanCode:Byte):Boolean;
function HIDKeyboardCheckReleased(Keyboard:PHIDKeyboardDevice;Keys:PHIDKeyboardKeys;ScanCode:Byte):Boolean;

function HIDKeyboardDeviceSetLEDs(Keyboard:PHIDKeyboardDevice;LEDs:Byte):LongWord;
function HIDKeyboardDeviceSetIdle(Keyboard:PHIDKeyboardDevice;Duration,ReportId:Byte):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {HID Keyboard specific variables}
 HIDKeyboardInitialized:Boolean;

 HIDKeyboardConsumer:PHIDConsumer; {HID Consumer interface (Set by HIDKeyboardInit)}

{==============================================================================}
{==============================================================================}
{Forward Declarations}
function HIDKeyboardCollectionBind(Device:PHIDDevice;Collection:PHIDCollection):LongWord; forward;
function HIDKeyboardCollectionUnbind(Device:PHIDDevice;Collection:PHIDCollection):LongWord; forward;

function HIDKeyboardReportReceive(Collection:PHIDCollection;ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure HIDKeyboardInit;
{Initialize the HID Keyboard unit and HID Keyboard driver}

{Note: Called only during system startup}
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if HIDKeyboardInitialized then Exit;

 if HID_REGISTER_KEYBOARD then
  begin
   {Create HID Keyboard Consumer}
   HIDKeyboardConsumer:=HIDConsumerCreate;
   if HIDKeyboardConsumer <> nil then
    begin
     {Update HID Keyboard Consumer}
     {Driver}
     HIDKeyboardConsumer.Driver.DriverName:=HID_KEYBOARD_CONSUMER_NAME;
     {HID}
     HIDKeyboardConsumer.CollectionBind:=HIDKeyboardCollectionBind;
     HIDKeyboardConsumer.CollectionUnbind:=HIDKeyboardCollectionUnbind;
     HIDKeyboardConsumer.ReportReceive:=HIDKeyboardReportReceive;

     {Register HID Keyboard Consumer}
     Status:=HIDConsumerRegister(HIDKeyboardConsumer);
     if Status <> ERROR_SUCCESS then
      begin
       if HID_LOG_ENABLED then HIDLogError(nil,'Keyboard: Failed to register HID Keyboard consumer: ' + ErrorToString(Status));

       {Destroy Consumer}
       HIDConsumerDestroy(HIDKeyboardConsumer);

       HIDKeyboardConsumer:=nil;
      end;
    end
   else
    begin
     if HID_LOG_ENABLED then HIDLogError(nil,'Keyboard: Failed to create HID Keyboard consumer');
    end;
  end;

 HIDKeyboardInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{HID Keyboard Functions}
function HIDKeyboardDeviceControl(Keyboard:PKeyboardDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
{Implementation of KeyboardDeviceControl API for HID Keyboard}
{Note: Not intended to be called directly by applications, use KeyboardDeviceControl instead}
var
 Status:LongWord;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Keyboard Attached}
 if Keyboard.KeyboardState <> KEYBOARD_STATE_ATTACHED then Exit;

 {Acquire the Lock}
 if MutexLock(Keyboard.Lock) = ERROR_SUCCESS then
  begin
   try
    case Request of
     KEYBOARD_CONTROL_GET_FLAG:begin
       {Get Flag}
       Argument2:=Ord(False);
       if (Keyboard.Device.DeviceFlags and Argument1) <> 0 then
        begin
         Argument2:=Ord(True);

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     KEYBOARD_CONTROL_SET_FLAG:begin
       {Set Flag}
       if (Argument1 and not(KEYBOARD_FLAG_MASK)) = 0 then
        begin
         Keyboard.Device.DeviceFlags:=(Keyboard.Device.DeviceFlags or Argument1);

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     KEYBOARD_CONTROL_CLEAR_FLAG:begin
       {Clear Flag}
       if (Argument1 and not(KEYBOARD_FLAG_MASK)) = 0 then
        begin
         Keyboard.Device.DeviceFlags:=(Keyboard.Device.DeviceFlags and not(Argument1));

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     KEYBOARD_CONTROL_FLUSH_BUFFER:begin
       {Flush Buffer}
       while Keyboard.Buffer.Count > 0 do
        begin
         {Wait for Data (Should not Block)}
         if SemaphoreWait(Keyboard.Buffer.Wait) = ERROR_SUCCESS then
          begin
           {Update Start}
           Keyboard.Buffer.Start:=(Keyboard.Buffer.Start + 1) mod KEYBOARD_BUFFER_SIZE;

           {Update Count}
           Dec(Keyboard.Buffer.Count);
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
     KEYBOARD_CONTROL_GET_LED:begin
       {Get LED}
       Argument2:=Ord(False);
       if (Keyboard.KeyboardLEDs and Argument1) <> 0 then
        begin
         Argument2:=Ord(True);

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     KEYBOARD_CONTROL_SET_LED:begin
       {Set LED}
       if (Argument1 and not(KEYBOARD_LED_MASK)) = 0 then
        begin
         Keyboard.KeyboardLEDs:=(Keyboard.KeyboardLEDs or Argument1);

         {Set LEDs}
         Status:=HIDKeyboardDeviceSetLEDs(PHIDKeyboardDevice(Keyboard),Keyboard.KeyboardLEDs);
         if (Status <> ERROR_SUCCESS) and (Status <> ERROR_NOT_SUPPORTED) then
          begin
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     KEYBOARD_CONTROL_CLEAR_LED:begin
       {Clear LED}
       if (Argument1 and not(KEYBOARD_LED_MASK)) = 0 then
        begin
         Keyboard.KeyboardLEDs:=(Keyboard.KeyboardLEDs and not(Argument1));

         {Set LEDs}
         Status:=HIDKeyboardDeviceSetLEDs(PHIDKeyboardDevice(Keyboard),Keyboard.KeyboardLEDs);
         if (Status <> ERROR_SUCCESS) and (Status <> ERROR_NOT_SUPPORTED) then
          begin
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     KEYBOARD_CONTROL_GET_REPEAT_RATE:begin
       {Get Repeat Rate}
       Argument2:=Keyboard.KeyboardRate;

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     KEYBOARD_CONTROL_SET_REPEAT_RATE:begin
       {Set Repeat Rate}
       Keyboard.KeyboardRate:=Argument1;

       {Set Idle}
       Definition:=PHIDKeyboardDevice(Keyboard).Definitions;
       while Definition <> nil do
        begin
         Status:=HIDKeyboardDeviceSetIdle(PHIDKeyboardDevice(Keyboard),Keyboard.KeyboardRate,Definition.Id);
         if Status <> ERROR_SUCCESS then
          begin
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;

         {Get Next Definition}
         Definition:=Definition.Next;
        end;

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     KEYBOARD_CONTROL_GET_REPEAT_DELAY:begin
       {Get Repeat Delay}
       Argument2:=Keyboard.KeyboardDelay;

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     KEYBOARD_CONTROL_SET_REPEAT_DELAY:begin
       {Set Repeat Delay}
       Keyboard.KeyboardDelay:=Argument1;

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
    end;
   finally
    {Release the Lock}
    MutexUnlock(Keyboard.Lock);
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
{HID Keyboard Helper Functions}
function HIDKeyboardCheckCollection(Collection:PHIDCollection):LongWord;
{Check if a HID collection is suitable for use as a keyboard device}
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
     HID_DESKTOP_KEYBOARD:begin
       {Check Flags}
       if Collection.Flags = HID_MAIN_COLLECTION_APPLICATION then Result:=ERROR_SUCCESS;
      end;
    end;
   end;
 end;
end;

{==============================================================================}

function HIDKeyboardCheckInputDefinition(Definition:PHIDDefinition):LongWord;
{Check if a HID definition is suitable for use as a keyboard input report}
var
 HasKeys:Boolean;
 HasModifiers:Boolean;
 Field:PHIDField;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Definition}
 if Definition = nil then Exit;

 {Set Defaults}
 HasKeys:=False;
 HasModifiers:=False;

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
    HID_PAGE_KEYBOARD_KEYPAD:begin
      {Check Keys}
      if (SmallInt(Field.Usage) >= SCAN_CODE_NONE) and (Field.Usage + Field.Count - 1 <= SCAN_CODE_RESERVED_255) then {Max is SCAN_CODE_KEYPAD_HEX}
       begin
        HasKeys:=True;
       end;

      {Check Modifiers}
      if (Field.Usage >= SCAN_CODE_LEFT_CTRL) and (Field.Usage + Field.Count - 1 <= SCAN_CODE_RIGHT_GUI) then
       begin
        HasModifiers:=True;
       end;
     end;
   end;

   {Get Next Field}
   Field:=Field.Next;
  end;

 {Check Result}
 if HasKeys and HasModifiers then Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDKeyboardCheckOutputDefinition(Definition:PHIDDefinition):LongWord;
{Check if a HID definition is suitable for use as a keyboard output (LED) report}
var
 HasLEDs:Boolean;
 Field:PHIDField;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Definition}
 if Definition = nil then Exit;

 {Set Defaults}
 HasLEDs:=False;

 {Set Result}
 Result:=ERROR_NOT_SUPPORTED;

 {Check Kind}
 if Definition.Kind <> HID_REPORT_OUTPUT then Exit;

 {Check Fields}
 Field:=Definition.Fields;
 while Field <> nil do
  begin
   {Check Page}
   case Field.Page of
    HID_PAGE_LED:begin
      {Check LEDs}
      if (Field.Usage >= HID_LED_NUM_LOCK) and (Field.Usage + Field.Count - 1 <= HID_LED_MUTE) then
       begin
        HasLEDs:=True;
       end;
     end;
   end;

   {Get Next Field}
   Field:=Field.Next;
  end;

 {Check Result}
 if HasLEDs then Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDKeyboardCheckPressed(Keyboard:PHIDKeyboardDevice;ScanCode:Byte):Boolean;
{Check if the passed scan code has been pressed (True if not pressed in last report)}
{Keyboard: The HID keyboard device to check for}
{ScanCode: The keyboard scan code to check}

{Note: Caller must hold the keyboard lock}
var
 Count:LongWord;
begin
 {}
 Result:=True;

 {Check Keyboard}
 if Keyboard = nil then Exit;

 for Count:=0 to HID_KEYBOARD_MAX_KEYS - 1 do {Up to 16 bytes of Keypress data}
  begin
   if Keyboard.LastKeys[Count] = ScanCode then
    begin
     Result:=False;
     Exit;
    end;
  end;
end;

{==============================================================================}

function HIDKeyboardCheckRepeated(Keyboard:PHIDKeyboardDevice;ScanCode:Byte):Boolean;
{Check if the passed scan code was the last key pressed and if the repeat delay has expired}
{Keyboard: The HID keyboard device to check for}
{ScanCode: The keyboard scan code to check}

{Note: Caller must hold the keyboard lock}
begin
 {}
 Result:=False;

 {Check Keyboard}
 if Keyboard = nil then Exit;

 if ScanCode = Keyboard.LastCode then
  begin
   if Keyboard.LastCount < Keyboard.Keyboard.KeyboardDelay then
    begin
     Inc(Keyboard.LastCount);
    end
   else
    begin
     Result:=True;
    end;
  end;
end;

{==============================================================================}

function HIDKeyboardCheckReleased(Keyboard:PHIDKeyboardDevice;Keys:PHIDKeyboardKeys;ScanCode:Byte):Boolean;
{Check if the passed scan code has been released (True if not pressed in current report)}
{Keyboard: The HID keyboard device to check for}
{Keys: The HID keyboard keys to compare against (Current)}
{ScanCode: The keyboard scan code to check}

{Note: Caller must hold the keyboard lock}
var
 Count:LongWord;
begin
 {}
 Result:=True;

 {Check Keyboard}
 if Keyboard = nil then Exit;

 {Check Keys}
 if Keys = nil then Exit;

 for Count:=0 to HID_KEYBOARD_MAX_KEYS - 1 do {Up to 16 bytes of Keypress data}
  begin
   if Keys[Count] = ScanCode then
    begin
     Result:=False;
     Exit;
    end;
  end;
end;

{==============================================================================}

function HIDKeyboardDeviceSetLEDs(Keyboard:PHIDKeyboardDevice;LEDs:Byte):LongWord;
{Set the state of the LEDs for a HID keyboard device}
{Keyboard: The HID keyboard device to set the LEDs for}
{LEDs: The LED state to set (eg KEYBOARD_LED_NUMLOCK)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Data:PByte;
 Size:LongWord;
 Field:PHIDField;
 Device:PHIDDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Keyboard}
 if Keyboard = nil then Exit;

 {Get Device}
 Device:=PHIDDevice(Keyboard.Keyboard.Device.DeviceData);
 if Device = nil then Exit;

 Result:=ERROR_NOT_SUPPORTED;

 {Check Definition}
 if Keyboard.LEDDefinition = nil then Exit;

 Result:=ERROR_OUTOFMEMORY;

 {Allocate Data}
 Size:=Keyboard.LEDDefinition.Size;
 Data:=AllocMem(Size);
 if Data = nil then Exit;
 try
  {Insert Fields}
  Field:=Keyboard.LEDDefinition.Fields;
  while Field <> nil do
   begin
    {Check Page}
    case Field.Page of
     HID_PAGE_LED:begin
       {Check Usage}
       case Field.Usage of
        HID_LED_NUM_LOCK:begin
          if (LEDs and KEYBOARD_LED_NUMLOCK) <> 0 then HIDInsertBitField(Field,Data,Size,True);
         end;
        HID_LED_CAPS_LOCK:begin
          if (LEDs and KEYBOARD_LED_CAPSLOCK) <> 0 then HIDInsertBitField(Field,Data,Size,True);
         end;
        HID_LED_SCROLL_LOCK:begin
          if (LEDs and KEYBOARD_LED_SCROLLLOCK) <> 0 then HIDInsertBitField(Field,Data,Size,True);
         end;
        HID_LED_COMPOSE:begin
          if (LEDs and KEYBOARD_LED_COMPOSE) <> 0 then HIDInsertBitField(Field,Data,Size,True);
         end;
        HID_LED_KANA:begin
          if (LEDs and KEYBOARD_LED_KANA) <> 0 then HIDInsertBitField(Field,Data,Size,True);
         end;
        {HID_LED_POWER}
        {HID_LED_SHIFT}
        {HID_LED_DO_NOT_DISTURB}
        {HID_LED_MUTE}
       end;
      end;
    end;

    {Get Next Field}
    Field:=Field.Next;
   end;

  {Set Report}
  Result:=HIDDeviceSetReport(Device,Keyboard.LEDDefinition.Kind,Keyboard.LEDDefinition.Id,Data,Size);
 finally
  {Free Data}
  FreeMem(Data);
 end;
end;

{==============================================================================}

function HIDKeyboardDeviceSetIdle(Keyboard:PHIDKeyboardDevice;Duration,ReportId:Byte):LongWord;
{Set the idle duration (Time between reports when no changes) for a HID keyboard device}
{Keyboard: The HID keyboard device to set the idle duration for}
{Duration: The idle duration to set (Milliseconds divided by 4)}
{ReportId: The report Id to set the idle duration for (eg HID_REPORTID_NONE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Device:PHIDDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Keyboard}
 if Keyboard = nil then Exit;

 {Get Device}
 Device:=PHIDDevice(Keyboard.Keyboard.Device.DeviceData);
 if Device = nil then Exit;

 {Set Idle}
 Result:=HIDDeviceSetIdle(Device,Duration,ReportId);
end;

{==============================================================================}
{==============================================================================}
{HID Keyboard Internal Functions}
function HIDKeyboardCollectionBind(Device:PHIDDevice;Collection:PHIDCollection):LongWord;
{Implementation of HIDCollectionBind API for HID Keyboard}
{Note: Not intended to be called directly by applications}
var
 MinId:Byte;
 MaxId:Byte;
 Index:LongWord;
 Status:LongWord;
 Keyboard:PHIDKeyboardDevice;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Collection}
 if Collection = nil then Exit;

 {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Device,'Keyboard: Attempting to bind HID collection (Name=' + DeviceGetName(@Device.Device) + ' Description=' + Device.Device.DeviceDescription + ' Usage=' + IntToHex(Collection.Page,4) + IntToHex(Collection.Usage,4) + ')');
 {$ENDIF}

 {Set Result}
 Result:=ERROR_NOT_SUPPORTED;

 {Check Collection}
 Status:=HIDKeyboardCheckCollection(Collection);
 if Status <> ERROR_SUCCESS then Exit;

 {Create Keyboard}
 Keyboard:=PHIDKeyboardDevice(KeyboardDeviceCreateEx(SizeOf(THIDKeyboardDevice)));
 if Keyboard = nil then
  begin
   if HID_LOG_ENABLED then HIDLogError(Device,'Keyboard: Failed to create new keyboard device');

   Exit;
  end;

 {Update Keyboard}
 {Device}
 Keyboard.Keyboard.Device.DeviceBus:=DEVICE_BUS_NONE;
 Keyboard.Keyboard.Device.DeviceType:=KEYBOARD_TYPE_NONE;
 if Device.Device.DeviceType = HID_TYPE_USB then
  begin
   Keyboard.Keyboard.Device.DeviceBus:=DEVICE_BUS_USB;
   Keyboard.Keyboard.Device.DeviceType:=KEYBOARD_TYPE_USB;
  end;
 Keyboard.Keyboard.Device.DeviceFlags:=Keyboard.Keyboard.Device.DeviceFlags; {Don't override defaults (was KEYBOARD_FLAG_NONE)}
 Keyboard.Keyboard.Device.DeviceData:=Device;
 Keyboard.Keyboard.Device.DeviceDescription:=Device.Device.DeviceDescription; {HID_KEYBOARD_DESCRIPTION}
 {Keyboard}
 Keyboard.Keyboard.KeyboardState:=KEYBOARD_STATE_ATTACHING;
 Keyboard.Keyboard.DeviceControl:=HIDKeyboardDeviceControl;
 {Driver}
 {HID}
 Keyboard.Collection:=Collection;

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
      if HIDKeyboardCheckInputDefinition(Definition) = ERROR_SUCCESS then
       begin
        {Link Definition}
        Definition.Next:=Keyboard.Definitions;
        Keyboard.Definitions:=Definition;
       end
      else
       begin
        {Free Definition}
        HIDFreeDefinition(Definition);
       end;
     end;

    {Output}
    Definition:=HIDAllocateDefinition(Device,Collection,HID_REPORT_OUTPUT,Index);
    if Definition <> nil then
     begin
      if HIDKeyboardCheckOutputDefinition(Definition) = ERROR_SUCCESS then
       begin
        {Set LED Definition}
        Keyboard.LEDDefinition:=Definition;
       end
      else
       begin
        {Free Definition}
        HIDFreeDefinition(Definition);
       end;
     end;
   end;

  {Check Definitions}
  if Keyboard.Definitions = nil then
   begin
    if HID_LOG_ENABLED then HIDLogError(Device,'Keyboard: Failed to allocate definitions');

    Exit;
   end;

  {Allocate Reports}
  Definition:=Keyboard.Definitions;
  while Definition <> nil do
   begin
    {Allocate Report}
    Status:=HIDDeviceAllocateReport(Device,Collection,Definition.Id,Definition.Size);
    if Status <> ERROR_SUCCESS then
     begin
      if HID_LOG_ENABLED then HIDLogError(Device,'Keyboard: Failed to allocate report id ' + IntToStr(Definition.Id) + ': ' + ErrorToString(Status));

      Exit;
     end;

    {Get Next Definition}
    Definition:=Definition.Next;
   end;

  {Register Keyboard}
  Status:=KeyboardDeviceRegister(@Keyboard.Keyboard);
  if Status <> ERROR_SUCCESS then
   begin
    if HID_LOG_ENABLED then HIDLogError(Device,'Keyboard: Failed to register new keyboard device: ' + ErrorToString(Status));

    Exit;
   end;

  {Set Repeat Rate}
  Definition:=Keyboard.Definitions;
  while Definition <> nil do
   begin
    Status:=HIDKeyboardDeviceSetIdle(Keyboard,Keyboard.Keyboard.KeyboardRate,Definition.Id);
    if Status <> ERROR_SUCCESS then
     begin
      if HID_LOG_ENABLED then HIDLogError(Device,'Keyboard: Failed to set idle rate: ' + ErrorToString(Status));

      Exit;
     end;

    {Get Next Definition}
    Definition:=Definition.Next;
   end;

  {Set LEDs}
  Status:=HIDKeyboardDeviceSetLEDs(Keyboard,Keyboard.Keyboard.KeyboardLEDs);
  if (Status <> ERROR_SUCCESS) and (Status <> ERROR_NOT_SUPPORTED) then
   begin
    if HID_LOG_ENABLED then HIDLogError(Device,'Keyboard: Failed to set LEDs: ' + ErrorToString(Status));

    Exit;
   end;

  {Check Interval}
  if USB_KEYBOARD_POLLING_INTERVAL > 0 then
   begin
    {Set Interval}
    HIDDeviceSetInterval(Device,USB_KEYBOARD_POLLING_INTERVAL);
   end;

  {Update Collection}
  Collection.PrivateData:=Keyboard;

  {Submit Reports}
  Definition:=Keyboard.Definitions;
  while Definition <> nil do
   begin
    Status:=HIDDeviceSubmitReport(Device,Definition.Id);
    if Status <> ERROR_SUCCESS then
     begin
      if HID_LOG_ENABLED then HIDLogError(Device,'Keyboard: Failed to submit report id ' + IntToStr(Definition.Id) + ': ' + ErrorToString(Status));

      Exit;
     end;

    {Get Next Definition}
    Definition:=Definition.Next;
   end;

  {Set State to Attached}
  if KeyboardDeviceSetState(@Keyboard.Keyboard,KEYBOARD_STATE_ATTACHED) <> ERROR_SUCCESS then Exit;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  if Result <> ERROR_SUCCESS then
   begin
    {Update Collection}
    Collection.PrivateData:=nil;

    {Deregister Keyboard}
    KeyboardDeviceDeregister(@Keyboard.Keyboard);

    {Cancel Reports / Release Reports / Free Definitions}
    Definition:=Keyboard.Definitions;
    while Definition <> nil do
     begin
      {Unlink Definition}
      Keyboard.Definitions:=Definition.Next;

      {Cancel Report}
      HIDDeviceCancelReport(Device,Definition.Id);

      {Release Report}
      HIDDeviceReleaseReport(Device,Definition.Id);

      {Free Definitions}
      HIDFreeDefinition(Definition);

      {Get Next Definition}
      Definition:=Keyboard.Definitions;
     end;

    {Destroy Keyboard}
    KeyboardDeviceDestroy(@Keyboard.Keyboard);
   end;
 end;
end;

{==============================================================================}

function HIDKeyboardCollectionUnbind(Device:PHIDDevice;Collection:PHIDCollection):LongWord;
{Implementation of HIDCollectionUnbind API for HID Keyboard}
{Note: Not intended to be called directly by applications}
var
 Keyboard:PHIDKeyboardDevice;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Collection}
 if Collection = nil then Exit;

 {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Device,'Keyboard: Unbinding HID collection (Name=' + DeviceGetName(@Device.Device) + ' Description=' + Device.Device.DeviceDescription + ' Usage=' + IntToHex(Collection.Page,4) + IntToHex(Collection.Usage,4) + ')');
 {$ENDIF}

 {Get Keyboard}
 Keyboard:=PHIDKeyboardDevice(Collection.PrivateData);
 if Keyboard = nil then Exit;
 if Keyboard.Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Set State to Detaching}
 Result:=ERROR_OPERATION_FAILED;
 if KeyboardDeviceSetState(@Keyboard.Keyboard,KEYBOARD_STATE_DETACHING) <> ERROR_SUCCESS then Exit;

 {Acquire the Lock}
 if MutexLock(Keyboard.Keyboard.Lock) <> ERROR_SUCCESS then Exit;

 {Cancel Reports}
 Definition:=Keyboard.Definitions;
 while Definition <> nil do
  begin
   {Cancel Report}
   HIDDeviceCancelReport(Device,Definition.Id);

   {Get Next Definition}
   Definition:=Definition.Next;
  end;

 {Release the Lock}
 MutexUnlock(Keyboard.Keyboard.Lock);

 {Set State to Detached}
 if KeyboardDeviceSetState(@Keyboard.Keyboard,KEYBOARD_STATE_DETACHED) <> ERROR_SUCCESS then Exit;

 {Update Collection}
 Collection.PrivateData:=nil;

 {Deregister Keyboard}
 if KeyboardDeviceDeregister(@Keyboard.Keyboard) <> ERROR_SUCCESS then Exit;

 {Release Reports / Free Definitions}
 Definition:=Keyboard.Definitions;
 while Definition <> nil do
  begin
   {Unlink Definition}
   Keyboard.Definitions:=Definition.Next;

   {Release Report}
   HIDDeviceReleaseReport(Device,Definition.Id);

   {Free Definitions}
   HIDFreeDefinition(Definition);

   {Get Next Definition}
   Definition:=Keyboard.Definitions;
  end;

 {Destroy Keyboard}
 KeyboardDeviceDestroy(@Keyboard.Keyboard);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HIDKeyboardReportReceive(Collection:PHIDCollection;ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord;
{Implementation of HIDReportReceive API for HID Keyboard}
{Note: Not intended to be called directly by applications}
var
 Index:Byte;
 Saved:Byte;
 Value:Boolean;
 Key:LongWord;
 Count:LongWord;
 LEDs:LongWord;
 KeyCode:Word;
 ScanCode:Byte;
 Status:LongWord;
 Counter:LongWord;
 Modifiers:LongWord;

 Keymap:TKeymapHandle;
 Keys:THIDKeyboardKeys;
 Keyboard:PHIDKeyboardDevice;
 KeyboardData:TKeyboardData;

 Field:PHIDField;
 Definition:PHIDDefinition;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Collection}
 if Collection = nil then Exit;

 {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(HID_DEBUG)}
 if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Keyboard: Report received (Id=' + IntToStr(ReportId) + ' Size=' + IntToStr(ReportSize) + ')');
 {$ENDIF}

 {Get Keyboard}
 Keyboard:=PHIDKeyboardDevice(Collection.PrivateData);
 if Keyboard <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Keyboard.Keyboard.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Keyboard.Keyboard.ReceiveCount);

      {Check State}
      if Keyboard.Keyboard.KeyboardState = KEYBOARD_STATE_DETACHING then
       begin
        {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(HID_DEBUG)}
        if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Keyboard: Detachment pending, report discarded');
        {$ENDIF}

        Result:=ERROR_SUCCESS;
        Exit;
       end;

      {Get Definition}
      Definition:=Keyboard.Definitions;
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
        Count:=0;
        Counter:=0;
        Modifiers:=0;
        Keymap:=KeymapGetDefault;
        LEDs:=Keyboard.Keyboard.KeyboardLEDs;
        FillChar(Keys,SizeOf(THIDKeyboardKeys),0);

        {Clear Keyboard Data}
        FillChar(KeyboardData,SizeOf(TKeyboardData),0);

        {Extract Fields}
        Field:=Definition.Fields;
        while Field <> nil do
         begin
          {Check Page}
          case Field.Page of
           HID_PAGE_KEYBOARD_KEYPAD:begin
             {Modifiers}
             case Field.Usage of
              SCAN_CODE_LEFT_CTRL:begin
                HIDExtractBitField(Field,ReportData,ReportSize,Value);
   
                if Value then Modifiers:=Modifiers or KEYBOARD_LEFT_CTRL;
               end;
              SCAN_CODE_LEFT_SHIFT:begin
                HIDExtractBitField(Field,ReportData,ReportSize,Value);
   
                if Value then Modifiers:=Modifiers or KEYBOARD_LEFT_SHIFT;
               end;
              SCAN_CODE_LEFT_ALT:begin
                HIDExtractBitField(Field,ReportData,ReportSize,Value);
   
                if Value then Modifiers:=Modifiers or KEYBOARD_LEFT_ALT;
               end;
              SCAN_CODE_LEFT_GUI:begin
                HIDExtractBitField(Field,ReportData,ReportSize,Value);
   
                if Value then Modifiers:=Modifiers or KEYBOARD_LEFT_GUI;
               end;
              SCAN_CODE_RIGHT_CTRL:begin
                HIDExtractBitField(Field,ReportData,ReportSize,Value);
   
                if Value then Modifiers:=Modifiers or KEYBOARD_RIGHT_CTRL;
               end;
              SCAN_CODE_RIGHT_SHIFT:begin
                HIDExtractBitField(Field,ReportData,ReportSize,Value);
   
                if Value then Modifiers:=Modifiers or KEYBOARD_RIGHT_SHIFT;
               end;
              SCAN_CODE_RIGHT_ALT:begin
                HIDExtractBitField(Field,ReportData,ReportSize,Value);
   
                if Value then Modifiers:=Modifiers or KEYBOARD_RIGHT_ALT;
               end;
              SCAN_CODE_RIGHT_GUI:begin
                HIDExtractBitField(Field,ReportData,ReportSize,Value);
   
                if Value then Modifiers:=Modifiers or KEYBOARD_RIGHT_GUI;
               end;
             end;
   
             {Keys}
             if (SmallInt(Field.Usage) >= SCAN_CODE_NONE) and (Field.Usage + Field.Count - 1 <= SCAN_CODE_RESERVED_255) then {Max is SCAN_CODE_KEYPAD_HEX}
              begin
               HIDExtractUnsignedField(Field,ReportData,ReportSize,Key);
   
               Keys[Count]:=Key;
               Inc(Count);
              end;
            end;
          end;

          {Get Next Field}
          Field:=Field.Next;
         end;

        {LED Modifiers}
        if Keyboard.Keyboard.KeyboardLEDs <> KEYBOARD_LED_NONE then
         begin
          if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_NUMLOCK) <> 0 then Modifiers:=Modifiers or KEYBOARD_NUM_LOCK;
          if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_CAPSLOCK) <> 0 then Modifiers:=Modifiers or KEYBOARD_CAPS_LOCK;
          if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_SCROLLLOCK) <> 0 then Modifiers:=Modifiers or KEYBOARD_SCROLL_LOCK;
          if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_COMPOSE) <> 0 then Modifiers:=Modifiers or KEYBOARD_COMPOSE;
          if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_KANA) <> 0 then Modifiers:=Modifiers or KEYBOARD_KANA;
         end;

        {Get Keymap Index}
        Index:=KEYMAP_INDEX_NORMAL;

        {Check for Shift}
        if (Modifiers and (KEYBOARD_LEFT_SHIFT or KEYBOARD_RIGHT_SHIFT)) <> 0 then
         begin
          Index:=KEYMAP_INDEX_SHIFT;

          {Check Shift behavior}
          if KEYBOARD_SHIFT_IS_CAPS_LOCK_OFF then
           begin
            {Check for Caps Lock}
            if (Modifiers and (KEYBOARD_CAPS_LOCK)) <> 0 then
             begin
              {Update LEDs}
              Keyboard.Keyboard.KeyboardLEDs:=Keyboard.Keyboard.KeyboardLEDs and not(KEYBOARD_LED_CAPSLOCK);
             end;
           end;
         end;

        {Check AltGr behavior}
        if KeymapCheckFlag(Keymap,KEYMAP_FLAG_ALTGR) then
         begin
          if not(KEYBOARD_CTRL_ALT_IS_ALTGR) then
           begin
            {Check for Right Alt}
            if (Modifiers and (KEYBOARD_RIGHT_ALT)) <> 0 then
             begin
              if Index <> KEYMAP_INDEX_SHIFT then Index:=KEYMAP_INDEX_ALTGR else Index:=KEYMAP_INDEX_SHIFT_ALTGR;
             end;
           end
          else
           begin
            {Check for Ctrl and Alt}
            if ((Modifiers and (KEYBOARD_LEFT_CTRL or KEYBOARD_RIGHT_CTRL)) <> 0) and ((Modifiers and (KEYBOARD_LEFT_ALT or KEYBOARD_RIGHT_ALT)) <> 0) then
             begin
              if Index <> KEYMAP_INDEX_SHIFT then Index:=KEYMAP_INDEX_ALTGR else Index:=KEYMAP_INDEX_SHIFT_ALTGR;
             end;
           end;

          {Check Keymap Index}
          if (Index = KEYMAP_INDEX_ALTGR) or (Index = KEYMAP_INDEX_SHIFT_ALTGR) then
           begin
            Modifiers:=Modifiers or KEYBOARD_ALTGR;
           end;
         end;

        {Save Keymap Index}
        Saved:=Index;

        {Check for Keys Pressed}
        for Count:=0 to HID_KEYBOARD_MAX_KEYS - 1 do
         begin
          {Load Keymap Index}
          Index:=Saved;

          {Get Scan Code}
          ScanCode:=Keys[Count];

          {Ignore SCAN_CODE_NONE to SCAN_CODE_ERROR}
          if ScanCode > SCAN_CODE_ERROR then
           begin
            {Check for Caps Lock Shifted Key}
            if KeymapCheckCapskey(Keymap,ScanCode) then
             begin
              {Check for Caps Lock}
              if (Modifiers and (KEYBOARD_CAPS_LOCK)) <> 0 then
               begin
                {Modify Normal and Shift}
                if Index = KEYMAP_INDEX_NORMAL then
                 begin
                  Index:=KEYMAP_INDEX_SHIFT;
                 end
                else if Index = KEYMAP_INDEX_SHIFT then
                 begin
                  Index:=KEYMAP_INDEX_NORMAL;
                 end
                {Modify AltGr and Shift}
                else if Index = KEYMAP_INDEX_ALTGR then
                 begin
                  Index:=KEYMAP_INDEX_SHIFT_ALTGR;
                 end
                else if Index = KEYMAP_INDEX_SHIFT_ALTGR then
                 begin
                  Index:=KEYMAP_INDEX_ALTGR;
                 end;
               end;
             end;

            {Check for Numeric Keypad Key}
            if (ScanCode >= SCAN_CODE_KEYPAD_FIRST) and (ScanCode <= SCAN_CODE_KEYPAD_LAST) then
             begin
              {Check for Num Lock}
              if (Modifiers and (KEYBOARD_NUM_LOCK)) <> 0 then
               begin
                {Check for Shift}
                if (Modifiers and (KEYBOARD_LEFT_SHIFT or KEYBOARD_RIGHT_SHIFT)) <> 0 then
                 begin
                  Index:=KEYMAP_INDEX_NORMAL;
                 end
                else
                 begin
                  Index:=KEYMAP_INDEX_SHIFT;
                 end;
               end
              else
               begin
                Index:=KEYMAP_INDEX_NORMAL;
               end;
             end;

            {Check Pressed}
            if HIDKeyboardCheckPressed(Keyboard,ScanCode) then
             begin
              {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(HID_DEBUG)}
              if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Keyboard: Key Pressed (ScanCode=' + IntToStr(ScanCode) + ' Modifiers=' + IntToHex(Modifiers,8) + ' Index=' + IntToStr(Index) + ')');
              {$ENDIF}

              {Check for NumLock / CapsLock / ScrollLock}
              if ScanCode = SCAN_CODE_NUMLOCK then
               begin
                {Update LEDs}
                Keyboard.Keyboard.KeyboardLEDs:=Keyboard.Keyboard.KeyboardLEDs xor KEYBOARD_LED_NUMLOCK;
               end
              else if ScanCode = SCAN_CODE_CAPSLOCK then
               begin
                {Update LEDs}
                Keyboard.Keyboard.KeyboardLEDs:=Keyboard.Keyboard.KeyboardLEDs xor KEYBOARD_LED_CAPSLOCK;
               end
              else if ScanCode = SCAN_CODE_SCROLLLOCK then
               begin
                {Update LEDs}
                Keyboard.Keyboard.KeyboardLEDs:=Keyboard.Keyboard.KeyboardLEDs xor KEYBOARD_LED_SCROLLLOCK;
               end
              else
               begin
                {Update Last}
                Keyboard.LastCode:=ScanCode;
                Keyboard.LastCount:=0;

                {Check for Deadkey}
                if (Keyboard.Keyboard.Code = SCAN_CODE_NONE) and KeymapCheckDeadkey(Keymap,ScanCode,Index) then
                 begin
                  {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(HID_DEBUG)}
                  if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Keyboard: Deadkey Pressed (ScanCode=' + IntToStr(ScanCode) + ' Modifiers=' + IntToHex(Modifiers,8) + ' Index=' + IntToStr(Index) + ')');
                  {$ENDIF}

                  {Update Deadkey}
                  Keyboard.Keyboard.Code:=ScanCode;
                  Keyboard.Keyboard.Index:=Index;
                  Keyboard.Keyboard.Modifiers:=Modifiers;

                  {Get Data}
                  KeyboardData.Modifiers:=Modifiers or KEYBOARD_KEYDOWN or KEYBOARD_DEADKEY;
                  KeyboardData.ScanCode:=ScanCode;
                  KeyboardData.KeyCode:=KeymapGetKeyCode(Keymap,ScanCode,Index);
                  KeyboardData.CharCode:=KeymapGetCharCode(Keymap,KeyboardData.KeyCode);
                  KeyboardData.CharUnicode:=KeymapGetCharUnicode(Keymap,KeyboardData.KeyCode);

                  {Insert Data}
                  if KeyboardInsertData(@Keyboard.Keyboard,@KeyboardData,True) = ERROR_SUCCESS then
                   begin
                    {Update Count}
                    Inc(Counter);
                   end;
                 end
                else
                 begin
                  {Check Deadkey}
                  KeyCode:=KEY_CODE_NONE;
                  if Keyboard.Keyboard.Code <> SCAN_CODE_NONE then
                   begin
                    {Resolve Deadkey}
                    if not KeymapResolveDeadkey(Keymap,Keyboard.Keyboard.Code,ScanCode,Keyboard.Keyboard.Index,Index,KeyCode) then
                     begin
                      {Get Data}
                      KeyboardData.Modifiers:=Keyboard.Keyboard.Modifiers or KEYBOARD_KEYDOWN;
                      KeyboardData.ScanCode:=Keyboard.Keyboard.Code;
                      KeyboardData.KeyCode:=KeymapGetKeyCode(Keymap,Keyboard.Keyboard.Code,Keyboard.Keyboard.Index);
                      KeyboardData.CharCode:=KeymapGetCharCode(Keymap,KeyboardData.KeyCode);
                      KeyboardData.CharUnicode:=KeymapGetCharUnicode(Keymap,KeyboardData.KeyCode);

                      {Insert Data}
                      if KeyboardInsertData(@Keyboard.Keyboard,@KeyboardData,True) = ERROR_SUCCESS then
                       begin
                        {Update Count}
                        Inc(Counter);
                       end;
                     end;
                   end;

                  {Reset Deadkey}
                  Keyboard.Keyboard.Code:=SCAN_CODE_NONE;

                  {Get Data}
                  KeyboardData.Modifiers:=Modifiers or KEYBOARD_KEYDOWN;
                  KeyboardData.ScanCode:=ScanCode;
                  KeyboardData.KeyCode:=KeymapGetKeyCode(Keymap,ScanCode,Index);
                  if KeyCode <> KEY_CODE_NONE then KeyboardData.KeyCode:=KeyCode;
                  KeyboardData.CharCode:=KeymapGetCharCode(Keymap,KeyboardData.KeyCode);
                  KeyboardData.CharUnicode:=KeymapGetCharUnicode(Keymap,KeyboardData.KeyCode);

                  {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(HID_DEBUG)}
                  if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Keyboard: Key Pressed (KeyCode=' + IntToHex(KeyboardData.KeyCode,4) + ' CharCode=' + IntToHex(Byte(KeyboardData.CharCode),2) + ' CharUnicode=' + IntToHex(Word(KeyboardData.CharUnicode),4) + ')');
                  {$ENDIF}

                  {Insert Data}
                  if KeyboardInsertData(@Keyboard.Keyboard,@KeyboardData,True) = ERROR_SUCCESS then
                   begin
                    {Update Count}
                    Inc(Counter);
                   end;
                 end;
               end;
             end
            else
             begin
              {Check Repeated}
              if HIDKeyboardCheckRepeated(Keyboard,ScanCode) then
               begin
                {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(HID_DEBUG)}
                if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Keyboard: Key Repeated (ScanCode=' + IntToStr(ScanCode) + ' Modifiers=' + IntToHex(Modifiers,8) + ' Index=' + IntToStr(Index) + ')');
                {$ENDIF}

                {Get Data}
                KeyboardData.Modifiers:=Modifiers or KEYBOARD_KEYREPEAT;
                KeyboardData.ScanCode:=ScanCode;
                KeyboardData.KeyCode:=KeymapGetKeyCode(Keymap,ScanCode,Index);
                KeyboardData.CharCode:=KeymapGetCharCode(Keymap,KeyboardData.KeyCode);
                KeyboardData.CharUnicode:=KeymapGetCharUnicode(Keymap,KeyboardData.KeyCode);

                {Insert Data}
                if KeyboardInsertData(@Keyboard.Keyboard,@KeyboardData,True) = ERROR_SUCCESS then
                 begin
                  {Update Count}
                  Inc(Counter);
                 end;
               end;
             end;
           end;
         end;

        {Check for Keys Released}
        for Count:=0 to HID_KEYBOARD_MAX_KEYS - 1 do
         begin
          {Load Keymap Index}
          Index:=Saved;

          {Get Scan Code}
          ScanCode:=Keyboard.LastKeys[Count];

          {Ignore SCAN_CODE_NONE to SCAN_CODE_ERROR}
          if ScanCode > SCAN_CODE_ERROR then
           begin
            {Check for Caps Lock Shifted Key}
            if KeymapCheckCapskey(Keymap,ScanCode) then
             begin
              {Check for Caps Lock}
              if (Modifiers and (KEYBOARD_CAPS_LOCK)) <> 0 then
               begin
                {Modify Normal and Shift}
                if Index = KEYMAP_INDEX_NORMAL then
                 begin
                  Index:=KEYMAP_INDEX_SHIFT;
                 end
                else if Index = KEYMAP_INDEX_SHIFT then
                 begin
                  Index:=KEYMAP_INDEX_NORMAL;
                 end
                {Modify AltGr and Shift}
                else if Index = KEYMAP_INDEX_ALTGR then
                 begin
                  Index:=KEYMAP_INDEX_SHIFT_ALTGR;
                 end
                else if Index = KEYMAP_INDEX_SHIFT_ALTGR then
                 begin
                  Index:=KEYMAP_INDEX_ALTGR;
                 end;
               end;
             end;

            {Check for Numeric Keypad Key}
            if (ScanCode >= SCAN_CODE_KEYPAD_FIRST) and (ScanCode <= SCAN_CODE_KEYPAD_LAST) then
             begin
              {Check for Num Lock}
              if (Modifiers and (KEYBOARD_NUM_LOCK)) <> 0 then
               begin
                {Check for Shift}
                if (Modifiers and (KEYBOARD_LEFT_SHIFT or KEYBOARD_RIGHT_SHIFT)) <> 0 then
                 begin
                  Index:=KEYMAP_INDEX_NORMAL;
                 end
                else
                 begin
                  Index:=KEYMAP_INDEX_SHIFT;
                 end;
               end
              else
               begin
                Index:=KEYMAP_INDEX_NORMAL;
               end;
             end;

            {Check Released}
            if HIDKeyboardCheckReleased(Keyboard,@Keys,ScanCode) then
             begin
              {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(HID_DEBUG)}
              if HID_LOG_ENABLED then HIDLogDebug(Collection.Device,'Keyboard: Key Released (ScanCode=' + IntToStr(ScanCode) + ' Modifiers=' + IntToHex(Modifiers,8) + ' Index=' + IntToStr(Index)+ ')');
              {$ENDIF}

              {Reset Last}
              Keyboard.LastCode:=SCAN_CODE_NONE;
              Keyboard.LastCount:=0;

              {Get Data}
              KeyboardData.Modifiers:=Modifiers or KEYBOARD_KEYUP;
              KeyboardData.ScanCode:=ScanCode;
              KeyboardData.KeyCode:=KeymapGetKeyCode(Keymap,ScanCode,Index);
              KeyboardData.CharCode:=KeymapGetCharCode(Keymap,KeyboardData.KeyCode);
              KeyboardData.CharUnicode:=KeymapGetCharUnicode(Keymap,KeyboardData.KeyCode);

              {Insert Data}
              if KeyboardInsertData(@Keyboard.Keyboard,@KeyboardData,True) = ERROR_SUCCESS then
               begin
                {Update Count}
                Inc(Counter);
               end;
             end;
           end;
         end;

        {Save Last Keys}
        System.Move(Keys[0],Keyboard.LastKeys[0],SizeOf(THIDKeyboardKeys));

        {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(HID_DEBUG)}
        if HID_LOG_ENABLED and (Counter > 0) then HIDLogDebug(Collection.Device,'Keyboard: Reported ' + IntToStr(Counter) + ' new keys');
        {$ENDIF}

        {Check LEDs}
        if LEDs <> Keyboard.Keyboard.KeyboardLEDs then
         begin
          {Update LEDs}
          Status:=HIDKeyboardDeviceSetLEDs(Keyboard,Keyboard.Keyboard.KeyboardLEDs);
          if (Status <> ERROR_SUCCESS) and (Status <> ERROR_NOT_SUPPORTED) then
           begin
            if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Keyboard: Failed to set LEDs: ' + ErrorToString(Status));
           end;
         end;
       end
      else
       begin
        if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Keyboard: Failed report request, no report definition or invalid size (Id=' + IntToStr(ReportId) + ' Size=' + IntToStr(ReportSize) + ')');

        {Update Statistics}
        Inc(Keyboard.Keyboard.ReceiveErrors);
       end;

      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Keyboard.Keyboard.Lock);
     end;
    end
   else
    begin
     if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Keyboard: Failed to acquire lock');
    end;
  end
 else
  begin
   if HID_LOG_ENABLED then HIDLogError(Collection.Device,'Keyboard: Report request invalid');
  end;
end;

{==============================================================================}
{==============================================================================}

initialization
 HIDKeyboardInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
