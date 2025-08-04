{
Ultibo USB Shell extension unit.

Copyright (C) 2020 - SoftOz Pty Ltd.

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



Shell USB
=========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit ShellUSB;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,USB,SysUtils,Classes,Ultibo,UltiboClasses,UltiboUtils,Shell;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {Shell USB specific constants}

 {Shell USB Command constants}
 SHELL_USB_COMMAND_USB = 'USB';

 {Shell USB Action constants}
 SHELL_USB_ACTION_START   = 'START';
 SHELL_USB_ACTION_STOP    = 'STOP';
 SHELL_USB_ACTION_LIST    = 'LIST';
 SHELL_USB_ACTION_TREE    = 'TREE';
 SHELL_USB_ACTION_INFO    = 'INFO';

 {Shell USB Item constants}
 SHELL_USB_ITEM_ALL       = 'ALL';
 SHELL_USB_ITEM_HOSTS     = 'HOSTS';
 SHELL_USB_ITEM_DEVICES   = 'DEVICES';
 SHELL_USB_ITEM_DRIVERS   = 'DRIVERS';

{==============================================================================}
type
 {Shell USB specific types}
 PShellUSBData = ^TShellUSBData;
 TShellUSBData = record
  Shell:TShell;
  Session:TShellSession;
  Parameters:TStrings;
  Data:Pointer;
 end;

{==============================================================================}
type
 {Shell USB specific classes}
 TShellUSB = class(TShellCommand)
 public
  {}
  constructor Create;
 private
  {Internal Variables}

  {Internal Methods}
  function USBFlagsToFlagNames(AFlags:LongWord):TStringList;
  function USBHostFlagsToFlagNames(AFlags:LongWord):TStringList;
 protected
  {Internal Variables}

  {Internal Methods}

 public
  {Public Properties}

  {Public Methods}
  function DoHelp(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoInfo(AShell:TShell;ASession:TShellSession):Boolean; override;
  function DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean; override;
 end;

{==============================================================================}
{var}
 {Shell USB specific variables}

{==============================================================================}
{Initialization Functions}
procedure ShellUSBInit;

{==============================================================================}
{Shell USB Functions}

{==============================================================================}
{Shell USB Helper Functions}
function ShellUSBLogDeviceCallback(Device:PUSBDevice;Data:Pointer):LongWord;
function ShellUSBLogTreeCallback(Device:PUSBDevice;Data:Pointer):LongWord;

function ShellUSBHostEnumerate(Host:PUSBHost;Data:Pointer):LongWord;
function ShellUSBDeviceEnumerate(Device:PUSBDevice;Data:Pointer):LongWord;
function ShellUSBDriverEnumerate(Driver:PUSBDriver;Data:Pointer):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Shell USB specific variables}
 ShellUSBInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{Forward Declarations}
procedure ShellUSBLogOutput(const AText:String;Data:Pointer); forward;

{==============================================================================}
{==============================================================================}
{TShellUSB}
function TShellUSB.USBFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 {Nothing}

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('USB_FLAG_NONE');
  end;
end;

{==============================================================================}

function TShellUSB.USBHostFlagsToFlagNames(AFlags:LongWord):TStringList;
begin
 {}
 Result:=TStringList.Create;

 {Check Flags}
 if (AFlags and USBHOST_FLAG_SHARED) = USBHOST_FLAG_SHARED then
  begin
   Result.Add('USBHOST_FLAG_SHARED');
  end;
 if (AFlags and USBHOST_FLAG_NOCACHE) = USBHOST_FLAG_NOCACHE then
  begin
   Result.Add('USBHOST_FLAG_NOCACHE');
  end;

 {Check Flags}
 if Result.Count = 0 then
  begin
   Result.Add('USBHOST_FLAG_NONE');
  end;
end;

{==============================================================================}

constructor TShellUSB.Create;
begin
 {}
 inherited Create;

 Name:=SHELL_USB_COMMAND_USB;
 Flags:=SHELL_COMMAND_FLAG_INFO or SHELL_COMMAND_FLAG_HELP;
end;

{==============================================================================}

function TShellUSB.DoHelp(AShell:TShell;ASession:TShellSession):Boolean;
begin
 {}
 Result:=False;

 {Check Shell}
 if AShell = nil then Exit;

 {Do Help}
 AShell.DoOutput(ASession,'List or display information about active USB hosts, devices and drivers');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,' ' + Name + ' LIST <ITEM>           (List all currently active USB hosts, devices and drivers)');
 AShell.DoOutput(ASession,' ' + Name + ' TREE                  (Display the connection tree of active USB hosts and devices)');
 AShell.DoOutput(ASession,' ' + Name + ' INFO <DEVICE or HOST> (Display information for the specified USB host or device)');
 AShell.DoOutput(ASession,'');
 //AShell.DoOutput(ASession,' ' + Name + ' START                 (Start the USB subsystem if not currently active)'); //To Do
 //AShell.DoOutput(ASession,' ' + Name + ' STOP                  (Stop the USB subsystem and remove all hosts and devices)'); //To Do
 //AShell.DoOutput(ASession,''); //To Do
 AShell.DoOutput(ASession,'   List Items:');
 AShell.DoOutput(ASession,'    ALL         - List all active USB hosts, devices and drivers');
 AShell.DoOutput(ASession,'    HOSTS       - List all registered USB hosts');
 AShell.DoOutput(ASession,'    DEVICES     - List all registered USB devices');
 AShell.DoOutput(ASession,'    DRIVERS     - List all registered USB drivers');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'   Examples:');
 AShell.DoOutput(ASession,'    ' + Name + ' LIST');
 AShell.DoOutput(ASession,'    ' + Name + ' LIST HOSTS');
 AShell.DoOutput(ASession,'    ' + Name + ' LIST DEVICES');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'    ' + Name + ' INFO USB2');
 AShell.DoOutput(ASession,'    ' + Name + ' INFO USBHost0');
 AShell.DoOutput(ASession,'    ' + Name + ' INFO HostId=0');
 AShell.DoOutput(ASession,'    ' + Name + ' INFO Device=USB1');
 AShell.DoOutput(ASession,'    ' + Name + ' INFO DeviceId=3');
 AShell.DoOutput(ASession,'');
 AShell.DoOutput(ASession,'   ' + Name + ' with no parameters is equivalent to LIST ALL');
 AShell.DoOutput(ASession,'   ' + Name + ' LIST is equivalent to LIST ALL');
 AShell.DoOutput(ASession,'');

 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TShellUSB.DoInfo(AShell:TShell;ASession:TShellSession):Boolean;
begin
 {}
 Result:=False;

 {Check Shell}
 if AShell = nil then Exit;

 {Do Info}
 Result:=AShell.DoOutput(ASession,'List or display information about active USB hosts, devices and drivers');
end;

{==============================================================================}

function TShellUSB.DoCommand(AShell:TShell;ASession:TShellSession;AParameters:TStrings):Boolean;

 function DisplayHost(AHost:PUSBHost):Boolean;
 var
  Count:LongWord;
  FlagNames:TStringList;
 begin
  {}
  Result:=False;

  if AHost = nil then Exit;

  {Get Flags Names}
  FlagNames:=USBHostFlagsToFlagNames(AHost.Device.DeviceFlags);

  AShell.DoOutput(ASession,'  Host Information');
  AShell.DoOutput(ASession,'  ----------------');
  AShell.DoOutput(ASession,'');
  AShell.DoOutput(ASession,'  Name:   ' + DeviceGetName(@AHost.Device));
  AShell.DoOutput(ASession,'  Type: ' + USBHostTypeToString(AHost.Device.DeviceType));
  AShell.DoOutput(ASession,'  Flags: ' + FlagNames.Strings[0]);

  {Check Flag Count}
  if FlagNames.Count > 1 then
   begin
    for Count:=1 to FlagNames.Count - 1 do
     begin
      {Add Flag Name}
      AShell.DoOutput(ASession,'         ' + FlagNames.Strings[Count]);
     end;
   end;

  AShell.DoOutput(ASession,'');
  AShell.DoOutput(ASession,'  Id: ' + IntToStr(AHost.HostId));
  AShell.DoOutput(ASession,'  State: ' + USBHostStateToString(AHost.HostState));
  AShell.DoOutput(ASession,'');
  AShell.DoOutput(ASession,'  Alignment: ' + IntToStr(AHost.Alignment));
  AShell.DoOutput(ASession,'  Multiplier: ' + IntToStr(AHost.Multiplier));
  AShell.DoOutput(ASession,'  Max Transfer: ' + IntToStr(AHost.MaxTransfer));
  AShell.DoOutput(ASession,'');
  AShell.DoOutput(ASession,'  Request Count: ' + IntToStr(AHost.RequestCount));
  AShell.DoOutput(ASession,'  Request Errors: ' + IntToStr(AHost.RequestErrors));
  AShell.DoOutput(ASession,'');

  FlagNames.Free;
 end;

 function DisplayDevice(ADevice:PUSBDevice):Boolean;
 var
  Count:LongWord;
  WorkBuffer:String;
  FlagNames:TStringList;
 begin
  {}
  Result:=False;

  if ADevice = nil then Exit;

  {Get Flags Names}
  FlagNames:=USBFlagsToFlagNames(ADevice.Device.DeviceFlags);

  AShell.DoOutput(ASession,'  Device Information');
  AShell.DoOutput(ASession,'  ------------------');
  AShell.DoOutput(ASession,'');
  AShell.DoOutput(ASession,'  Name:   ' + DeviceGetName(@ADevice.Device));
  AShell.DoOutput(ASession,'  Type: ' + USBDeviceTypeToString(ADevice.Device.DeviceType));
  AShell.DoOutput(ASession,'  Flags: ' + FlagNames.Strings[0]);

  {Check Flag Count}
  if FlagNames.Count > 1 then
   begin
    for Count:=1 to FlagNames.Count - 1 do
     begin
      {Add Flag Name}
      AShell.DoOutput(ASession,'         ' + FlagNames.Strings[Count]);
     end;
   end;

  AShell.DoOutput(ASession,'');
  AShell.DoOutput(ASession,'  Id: ' + IntToStr(ADevice.USBId));
  AShell.DoOutput(ASession,'  State: ' + USBDeviceStateToString(ADevice.USBState));
  AShell.DoOutput(ASession,'  Status: ' + USBDeviceStatusToString(ADevice.USBStatus));
  AShell.DoOutput(ASession,'');

  AShell.DoOutput(ASession,'  Address: ' + IntToStr(ADevice.Address));
  AShell.DoOutput(ASession,'  Speed: ' + USBSpeedToStringAlt(ADevice.Speed));
  AShell.DoOutput(ASession,'  Depth: ' + IntToStr(ADevice.Depth));
  AShell.DoOutput(ASession,'  Port Number: ' + IntToStr(ADevice.PortNumber));
  AShell.DoOutput(ASession,'  Configuration Value: ' + IntToStr(ADevice.ConfigurationValue));
  AShell.DoOutput(ASession,'');

  WorkBuffer:='';
  if ADevice.Parent <> nil then WorkBuffer:=DeviceGetName(@ADevice.Parent.Device);
  AShell.DoOutput(ASession,'  Parent: ' + WorkBuffer);

  WorkBuffer:='';
  if ADevice.Driver <> nil then WorkBuffer:=DriverGetName(@ADevice.Driver.Driver);
  AShell.DoOutput(ASession,'  Driver: ' + WorkBuffer);

  AShell.DoOutput(ASession,'');
  AShell.DoOutput(ASession,'  Product: ' + ADevice.Product);
  AShell.DoOutput(ASession,'  Manufacturer: ' + ADevice.Manufacturer);
  AShell.DoOutput(ASession,'  SerialNumber: ' + ADevice.SerialNumber);
  AShell.DoOutput(ASession,'');
  AShell.DoOutput(ASession,'  Request Count: ' + IntToStr(ADevice.RequestCount));
  AShell.DoOutput(ASession,'  Request Errors: ' + IntToStr(ADevice.RequestErrors));
  AShell.DoOutput(ASession,'');
  AShell.DoOutput(ASession,'  Class: ' + USBClassCodeToString(ADevice.Descriptor.bDeviceClass));
  AShell.DoOutput(ASession,'  VID/PID: ' + IntToHex(ADevice.Descriptor.idVendor,4) + ':' + IntToHex(ADevice.Descriptor.idProduct,4));
  AShell.DoOutput(ASession,'');

  AShell.DoOutput(ASession,'  Device Descriptors');
  AShell.DoOutput(ASession,'  ------------------');
  AShell.DoOutput(ASession,'');

  FlagNames.Free;
 end;

var
 Item:String;
 Action:String;
 Match:Boolean;
 HostId:String;
 HostName:String;
 DeviceId:String;
 DeviceName:String;
 Host:PUSBHost;
 Device:PUSBDevice;
 Data:TShellUSBData;
 WorkBuffer:String;
begin
 {}
 Result:=False;

 {Check Shell}
 if AShell = nil then Exit;

 {Check Parameters}
 if AParameters = nil then Exit;

 {Setup Data}
 Data.Shell:=AShell;
 Data.Session:=ASession;
 Data.Parameters:=AParameters;
 Data.Data:=nil;

 {Get Action}
 Action:=AShell.ParameterIndex(0,AParameters);

 {Check Action}
 if (Length(Action) = 0) or (Uppercase(Action) = SHELL_USB_ACTION_LIST) then
  begin
   AShell.DoOutput(ASession,'USB List');
   AShell.DoOutput(ASession,'--------');
   AShell.DoOutput(ASession,'');

   {Get Item}
   Item:=AShell.ParameterIndex(1,AParameters);

   {Check Item}
   {Hosts}
   if (Length(Item) = 0) or (Uppercase(Item) = SHELL_USB_ITEM_ALL) or (Uppercase(Item) = SHELL_USB_ITEM_HOSTS) then
    begin
     AShell.DoOutput(ASession,'  Hosts');
     AShell.DoOutput(ASession,'  -----');
     AShell.DoOutput(ASession,'');
     WorkBuffer:='';
     AShell.AddOutput(WorkBuffer,2,'Name');
     AShell.AddOutput(WorkBuffer,20,'Type');
     AShell.AddOutput(WorkBuffer,60,'State');
     AShell.DoOutput(ASession,WorkBuffer);
     AShell.DoOutput(ASession,'');

     {Enumerate USB Hosts}
     USBHostEnumerate(ShellUSBHostEnumerate,@Data);

     if Uppercase(Item) <> SHELL_USB_ITEM_HOSTS then AShell.DoOutput(ASession,'');

     {Return Result}
     Result:=True;
    end;
   {Devices}
   if (Length(Item) = 0) or (Uppercase(Item) = SHELL_USB_ITEM_ALL) or (Uppercase(Item) = SHELL_USB_ITEM_DEVICES) then
    begin
     AShell.DoOutput(ASession,'  Devices');
     AShell.DoOutput(ASession,'  -------');
     AShell.DoOutput(ASession,'');
     WorkBuffer:='';
     AShell.AddOutput(WorkBuffer,2,'Name');
     AShell.AddOutput(WorkBuffer,20,'Class');
     AShell.AddOutput(WorkBuffer,60,'Status');
     AShell.DoOutput(ASession,WorkBuffer);
     AShell.DoOutput(ASession,'');

     {Enumerate USB Devices}
     USBDeviceEnumerate(ShellUSBDeviceEnumerate,@Data);

     if Uppercase(Item) <> SHELL_USB_ITEM_DEVICES then AShell.DoOutput(ASession,'');

     {Return Result}
     Result:=True;
    end;
   {Drivers}
   if (Length(Item) = 0) or (Uppercase(Item) = SHELL_USB_ITEM_ALL) or (Uppercase(Item) = SHELL_USB_ITEM_DRIVERS) then
    begin
     AShell.DoOutput(ASession,'  Drivers');
     AShell.DoOutput(ASession,'  -------');
     AShell.DoOutput(ASession,'');
     WorkBuffer:='';
     AShell.AddOutput(WorkBuffer,2,'Name');
     AShell.AddOutput(WorkBuffer,60,'State');
     AShell.DoOutput(ASession,WorkBuffer);
     AShell.DoOutput(ASession,'');

     {Enumerate USB Drivers}
     USBDriverEnumerate(ShellUSBDriverEnumerate,@Data);

     if Uppercase(Item) <> SHELL_USB_ITEM_DRIVERS then AShell.DoOutput(ASession,'');

     {Return Result}
     Result:=True;
    end;

   if not Result then
    begin
     {Show Error}
     Result:=AShell.DoError(ASession);
    end;
  end
 else if Uppercase(Action) = SHELL_USB_ACTION_TREE then
  begin
   AShell.DoOutput(ASession,'USB Tree');
   AShell.DoOutput(ASession,'--------');

   {Display Tree}
   USBLogDevicesEx(nil,ShellUSBLogOutput,nil,ShellUSBLogTreeCallback,@Data);

   AShell.DoOutput(ASession,'');

   {Return Result}
   Result:=True;
  end
 else if Uppercase(Action) = SHELL_USB_ACTION_INFO then
  begin
   AShell.DoOutput(ASession,'USB Info');
   AShell.DoOutput(ASession,'--------');
   AShell.DoOutput(ASession,'');

   Match:=False;

   {Get Host Id}
   HostId:=AShell.ParameterValue('HOSTID',AParameters);
   if Length(HostId) > 0 then Match:=True;

   {Get Host Name}
   if not Match then HostName:=AShell.ParameterValue('HOST',AParameters);
   if Length(HostName) > 0 then Match:=True;

   {Get Device Id}
   if not Match then DeviceId:=AShell.ParameterValue('DEVICEID',AParameters);
   if Length(DeviceId) > 0 then Match:=True;

   {Get Device Name}
   if not Match then DeviceName:=AShell.ParameterValue('DEVICE',AParameters);
   if Length(DeviceName) > 0 then Match:=True;

   {Get Item}
   if not Match then Item:=AShell.ParameterIndex(1,AParameters);
   if Length(Item) > 0 then Match:=True;

   if Match then
    begin
     {Check Host}
     if (Length(HostId) > 0) or (Length(HostName) > 0) then
      begin
       Host:=nil;

       {Check Id}
       if Length(HostId) > 0 then
        begin
         Host:=USBHostFind(StrToIntDef(HostId,Integer(DEVICE_ID_ANY)));
        end
       {Check Name}
       else if Length(HostName) > 0 then
        begin
         Host:=USBHostFindByName(HostName);
        end;

       {Display Host}
       if Host <> nil then
        begin
         DisplayHost(Host);
        end;
      end
     {Check Device}
     else if (Length(DeviceId) > 0) or (Length(DeviceName) > 0) then
      begin
       Device:=nil;

       {Check Id}
       if Length(DeviceId) > 0 then
        begin
         Device:=USBDeviceFind(StrToIntDef(DeviceId,Integer(DEVICE_ID_ANY)));
        end
       {Check Name}
       else if Length(DeviceName) > 0 then
        begin
         Device:=USBDeviceFindByName(DeviceName);
        end;

       {Display Device}
       if Device <> nil then
        begin
         DisplayDevice(Device);

         USBLogDevicesEx(Device,ShellUSBLogOutput,ShellUSBLogDeviceCallback,nil,@Data);
        end;
      end
     {Check Item}
     else if Length(Item) > 0 then
      begin
       {Check Host}
       Host:=USBHostFindByName(Item);
       if Host <> nil then
        begin
         DisplayHost(Host);
        end
       else
        begin
         {Check Device}
         Device:=USBDeviceFindByName(Item);
         if Device <> nil then
          begin
           DisplayDevice(Device);

           USBLogDevicesEx(Device,ShellUSBLogOutput,ShellUSBLogDeviceCallback,nil,@Data);
          end
         else
          begin
           AShell.DoOutput(ASession,'Host or Device "' + Item + '" not found');
          end;
        end;
      end
     else
      begin
       AShell.DoOutput(ASession,'Host or Device name or ID not supplied');
      end;
    end
   else
    begin
     AShell.DoOutput(ASession,'Host or Device name or ID not supplied');
    end;

   {Return Result}
   Result:=True;
  end
 else if Uppercase(Action) = SHELL_USB_ACTION_START then
  begin

   //To Do

   {Return Result}
   Result:=True;
  end
 else if Uppercase(Action) = SHELL_USB_ACTION_STOP then
  begin

   //To Do

   {Return Result}
   Result:=True;
  end
 else
  begin
   {Show Error}
   Result:=AShell.DoError(ASession);
  end;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ShellUSBInit;
begin
 {}
 {Check Initialized}
 if ShellUSBInitialized then Exit;

 {Register USB Commands}
 ShellRegisterCommand(TShellUSB.Create);

 ShellUSBInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Shell USB Functions}

{==============================================================================}
{==============================================================================}
{Shell USB Helper Functions}
procedure ShellUSBLogOutput(const AText:String;Data:Pointer);
var
 Shell:TShell;
 Session:TShellSession;
begin
 {}
 {Check Data}
 if Data = nil then Exit;

 {Get Shell}
 Shell:=PShellUSBData(Data).Shell;
 if Shell = nil then Exit;

 {Get Session}
 Session:=PShellUSBData(Data).Session;
 if Session = nil then Exit;

 {Send Output}
 Shell.DoOutput(Session,' ' + AText);
end;

{==============================================================================}

function ShellUSBLogDeviceCallback(Device:PUSBDevice;Data:Pointer):LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 ShellUSBLogOutput('[USB Device Id: ' + IntToStr(Device.USBId) + ' Address: ' + IntToStr(Device.Address) + ']',Data);

 USBLogDeviceDescriptor(Device,Device.Descriptor,ShellUSBLogOutput,Data);
 USBLogDeviceConfiguration(Device,ShellUSBLogOutput,Data);

 ShellUSBLogOutput('',Data);

 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function ShellUSBLogTreeCallback(Device:PUSBDevice;Data:Pointer):LongWord;
const
 SHELL_USB_TREE_SPACES_PER_LEVEL = 2;
 SHELL_USB_TREE_LINES_PER_PORT = 2;

var
 Count:Integer;
 WorkBuffer:String;
 LinesCount:Integer;
 SpacesCount:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Output Diagram}
 WorkBuffer:='';
 if Device.Depth <> 0 then
  begin
   SpacesCount:=(Device.Depth - 1) * (SHELL_USB_TREE_SPACES_PER_LEVEL + SHELL_USB_TREE_SPACES_PER_LEVEL - 1);

   if SHELL_USB_TREE_LINES_PER_PORT > 1 then
    begin
     for LinesCount:=0 to SHELL_USB_TREE_LINES_PER_PORT - 2 do
      begin
       WorkBuffer:=StringOfChar(' ',SpacesCount) + '|';

       ShellUSBLogOutput(WorkBuffer,Data);
      end;
    end;

   WorkBuffer:=StringOfChar(' ',SpacesCount) + '|' + StringOfChar('_',SHELL_USB_TREE_SPACES_PER_LEVEL);
  end;

 {Output Device}
 WorkBuffer:=WorkBuffer + 'Id: ' + IntToStr(Device.USBId) + ' / Addr: ' + IntToStr(Device.Address) + ' [' + USBDeviceToString(Device) + ']';
 ShellUSBLogOutput(WorkBuffer,Data);

 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function ShellUSBHostEnumerate(Host:PUSBHost;Data:Pointer):LongWord;
var
 Shell:TShell;
 Session:TShellSession;
 WorkBuffer:String;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Host}
 if Host = nil then Exit;

 {Check Data}
 if Data = nil then Exit;

 {Get Shell}
 Shell:=PShellUSBData(Data).Shell;
 if Shell = nil then Exit;

 {Get Session}
 Session:=PShellUSBData(Data).Session;
 if Session = nil then Exit;

 {Send Output}
 WorkBuffer:='';
 Shell.AddOutput(WorkBuffer,2,DeviceGetName(@Host.Device));
 Shell.AddOutput(WorkBuffer,20,USBHostStateToString(Host.HostState));
 Shell.AddOutput(WorkBuffer,60,USBHostTypeToString(Host.Device.DeviceType));
 Shell.DoOutput(Session,WorkBuffer);

 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function ShellUSBDeviceEnumerate(Device:PUSBDevice;Data:Pointer):LongWord;
var
 Shell:TShell;
 Session:TShellSession;
 WorkBuffer:String;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Data}
 if Data = nil then Exit;

 {Get Shell}
 Shell:=PShellUSBData(Data).Shell;
 if Shell = nil then Exit;

 {Get Session}
 Session:=PShellUSBData(Data).Session;
 if Session = nil then Exit;

 {Send Output}
 WorkBuffer:='';
 Shell.AddOutput(WorkBuffer,2,DeviceGetName(@Device.Device));
 Shell.AddOutput(WorkBuffer,20,USBClassCodeToString(Device.Descriptor.bDeviceClass));
 Shell.AddOutput(WorkBuffer,60,USBDeviceStatusToString(Device.USBStatus));
 Shell.DoOutput(Session,WorkBuffer);

 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function ShellUSBDriverEnumerate(Driver:PUSBDriver;Data:Pointer):LongWord;
var
 Shell:TShell;
 Session:TShellSession;
 WorkBuffer:String;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Driver}
 if Driver = nil then Exit;

 {Check Data}
 if Data = nil then Exit;

 {Get Shell}
 Shell:=PShellUSBData(Data).Shell;
 if Shell = nil then Exit;

 {Get Session}
 Session:=PShellUSBData(Data).Session;
 if Session = nil then Exit;

 {Send Output}
 WorkBuffer:='';
 Shell.AddOutput(WorkBuffer,2,DriverGetName(@Driver.Driver));
 Shell.AddOutput(WorkBuffer,60,DriverStateToString(Driver.Driver.DriverState));
 Shell.DoOutput(Session,WorkBuffer);

 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}
{==============================================================================}

initialization
 ShellUSBInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

