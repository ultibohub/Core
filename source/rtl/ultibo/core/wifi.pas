{
Ultibo WiFi (WPA) interface unit.

Copyright (C) 2016 - SoftOz Pty Ltd.

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

 Based on information from sources of WPA supplicant, the Linux kernel and others


WiFi
====

 IEEE80211 - https://en.wikipedia.org/wiki/IEEE_802.11
 
 WEP - https://en.wikipedia.org/wiki/Wired_Equivalent_Privacy
 
 WPA - https://en.wikipedia.org/wiki/Wi-Fi_Protected_Access
 
 WPA2 - https://en.wikipedia.org/wiki/IEEE_802.11i-2004
 
 MIC - https://en.wikipedia.org/wiki/Message_authentication_code
 
 TKIP - https://en.wikipedia.org/wiki/Temporal_Key_Integrity_Protocol
 
 CCMP - https://en.wikipedia.org/wiki/CCMP
 
 EAPOL - https://en.wikipedia.org/wiki/IEEE_802.1X
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit WiFi;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,Platform,Threads,Devices,Network,Transport,Crypto,SysUtils;

//To Do //See: http://w1.fi/wpa_supplicant/
        //See: http://w1.fi/wpa_supplicant/devel/
        //See: http://w1.fi/wpa_supplicant/devel/porting.html
        //See: http://w1.fi/wpa_supplicant/devel/driver_wrapper.html
        //See: http://w1.fi/wpa_supplicant/devel/code_structure.html
        //See: http://w1.fi/cgit
                      
//To Do //In general terms WPA appears to the network stack as a Transport layer
        //sending and receiving 2 specific packet types:
        //EAP-over-LAN (EAPOL) $888E and RSN pre-authentication $88C7
                      
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {WiFi specific constants}
 WIFI_DEVICE_TIMER_INTERVAL = 500; {Timer interval for new device additions}
 
 EAPOL_TRANSPORT_NAME = 'EAPOL';
 RSN_TRANSPORT_NAME = 'RSN';

 {WiFi Device States}
 //To Do
 
 {WiFi Device Status}
 //To Do
 
{==============================================================================}
type
 {WiFi specific types}
 PWiFiDeviceEvent = ^TWiFiDeviceEvent;
 TWiFiDeviceEvent = record
  Timer:TTimerHandle;
  Device:PNetworkDevice;
 end;
 
 {WiFi Device}
 PWiFiDevice = ^TWiFiDevice;
 
 {WiFi Device Methods}
 TWiFiDeviceStart = function(WiFi:PWiFiDevice):LongWord;
 TWiFiDeviceStop = function(WiFi:PWiFiDevice):LongWord;
 //To Do
 
 TWiFiDevice = record
  {Network Properties}
  Network:TNetworkDevice;
  {WiFi Properties}
  WiFiState:LongWord;               
  WiFiStatus:LongWord;              
  DeviceStart:TWiFiDeviceStart;
  DeviceStop:TWiFiDeviceStop;
  //To Do
  {Driver Properties}
  //To Do
 end; 

{==============================================================================}
type
 {WiFi specific classes}
 TWiFiAdapter = class(TNetworkAdapter)
   constructor Create(AManager:TAdapterManager;ADevice:PNetworkDevice;const AName:String);
  private
   {Internal Variables}

   {Status Variables}
   FDefaultAddress:THardwareAddress;
   FHardwareAddress:THardwareAddress;
   FBroadcastAddress:THardwareAddress;
   FMulticastAddresses:TMulticastAddresses;
  protected
   {Inherited Methods}

  public
   {}
   function AddTransport(APacketType,AFrameType:Word;const APacketName:String;APacketHandler:TAdapterPacketHandler):THandle; override;
   function RemoveTransport(AHandle:THandle;APacketType:Word):Boolean; override;

   function GetMTU(AHandle:THandle):Word; override;

   function SendPacket(AHandle:THandle;ADest:Pointer;APacket:PPacketFragment;ASize:Integer):Boolean; override;

   function GetDefaultAddress(AHandle:THandle):THardwareAddress; override;
   function GetHardwareAddress(AHandle:THandle):THardwareAddress; override;
   function SetHardwareAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean; override;
   function GetBroadcastAddress(AHandle:THandle):THardwareAddress; override;
   function GetMulticastAddresses(AHandle:THandle):TMulticastAddresses; override;

   function AddMulticastAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean; override;
   function RemoveMulticastAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean; override;
   
   function StartAdapter:Boolean; override;
   function StopAdapter:Boolean; override;
   function ProcessAdapter:Boolean; override;
 end;
 
 TEAPOLTransportAdapter = class(TTransportAdapter)
   constructor Create;
  private
   {Internal Variables}
   
  public
   {Status Variables}
   
 end;
 
 TEAPOLTransport = class(TNetworkTransport)
   constructor Create(AManager:TTransportManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}
 
   {Status Variables}

   {Internal Methods}
   function PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;

  protected
   {Inherited Methods}

   {Internal Methods}
   
  public
   {Public Methods}
   function AddAdapter(AAdapter:TNetworkAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean; override;
   function RemoveAdapter(AAdapter:TNetworkAdapter):Boolean; override;

   function StartTransport:Boolean; override;
   function StopTransport:Boolean; override;
   function ProcessTransport:Boolean; override;

   function BindTransport(AAdapter:TNetworkAdapter):Boolean; override;
   function UnbindTransport(AAdapter:TNetworkAdapter):Boolean; override;
   
   {EAPOL Methods}
   //To Do
 end;

 TRSNTransportAdapter = class(TTransportAdapter)
   constructor Create;
  private
   {Internal Variables}
   
  public
   {Status Variables}
   
 end;
 
 TRSNTransport = class(TNetworkTransport)
   constructor Create(AManager:TTransportManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}
 
   {Status Variables}

   {Internal Methods}
   function PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;

  protected
   {Inherited Methods}

   {Internal Methods}
   
  public
   {Public Methods}
   function AddAdapter(AAdapter:TNetworkAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean; override;
   function RemoveAdapter(AAdapter:TNetworkAdapter):Boolean; override;

   function StartTransport:Boolean; override;
   function StopTransport:Boolean; override;
   function ProcessTransport:Boolean; override;

   function BindTransport(AAdapter:TNetworkAdapter):Boolean; override;
   function UnbindTransport(AAdapter:TNetworkAdapter):Boolean; override;
   
   {RSN Methods}
   //To Do
 end;
 
{==============================================================================}
{var}
 {WiFi specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure WiFiInit;
function WiFiStart(Data:Pointer;Event:LongWord):LongWord;
function WiFiStop(Data:Pointer;Event:LongWord):LongWord;

function WiFiStartCompleted:Boolean;

{==============================================================================}
{WiFi Functions}
function WiFiDeviceStart(WiFi:PWiFiDevice):LongWord;
function WiFiDeviceStop(WiFi:PWiFiDevice):LongWord;

{==============================================================================}
{WiFi Helper Functions}
procedure WiFiNetworkDeviceAdd(Event:PWiFiDeviceEvent);
function WiFiNetworkDeviceRemove(Network:PNetworkDevice):LongWord;

function WiFiNetworkDeviceEnum(Network:PNetworkDevice;Data:Pointer):LongWord;
function WiFiNetworkDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {WiFi specific variables}
 WiFiInitialized:Boolean;
 WiFiStarted:Boolean;
 
{==============================================================================}
{==============================================================================}
{TWiFiAdapter}
constructor TWiFiAdapter.Create(AManager:TAdapterManager;ADevice:PNetworkDevice;const AName:String);
begin
 {}
 inherited Create(AManager,ADevice,AName);
 {Set Defaults}
 FAdapterType:=ADAPTER_TYPE_WIRELESS;
 FillChar(FDefaultAddress,SizeOf(THardwareAddress),0);
 FillChar(FHardwareAddress,SizeOf(THardwareAddress),0);
 FillChar(FBroadcastAddress,SizeOf(THardwareAddress),0);
 FillChar(FMulticastAddresses,SizeOf(TMulticastAddresses),0);
end;
 
{==============================================================================}

function TWiFiAdapter.AddTransport(APacketType,AFrameType:Word;const APacketName:String;APacketHandler:TAdapterPacketHandler):THandle;
var
 Transport:TAdapterTransport;
begin
 {}
 ReaderLock;
 try
  Result:=INVALID_HANDLE_VALUE;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter: AddTransport (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Packet = ' + PacketTypeToString(APacketType));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Frame = ' + FrameTypeToString(AFrameType));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Name = ' + APacketName);
  {$ENDIF}
  
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;
  
  {Check Device}
  if FDevice = nil then Exit;
  
  {Get Transport}
  Transport:=TAdapterTransport(GetTransportByType(APacketType,AFrameType,False,NETWORK_LOCK_NONE)); {Do not lock}
  if Transport <> nil then Exit;
 
  {Check Frame Type}
  case AFrameType of 
   FRAME_TYPE_ETHERNET_II:begin
     {Check Media Type}
     if FMediaType <> MEDIA_TYPE_ETHERNET then Exit;
    end;
   else
    begin
     {Invalid}
     Exit;
    end;
  end;
  
  {Create Transport}
  Transport:=TAdapterTransport.Create;
  Transport.FrameType:=AFrameType;
  Transport.PacketType:=APacketType;
  Transport.PacketName:=APacketName;
  Transport.PacketHandler:=APacketHandler;
 
  {Acquire Lock}
  FTransports.WriterLock;
  try
   {Add Transport}
   FTransports.Add(Transport);
 
   {Return Result}
   Result:=THandle(Transport);
  finally
   {Release Lock}
   FTransports.WriterUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;
    
{==============================================================================}

function TWiFiAdapter.RemoveTransport(AHandle:THandle;APacketType:Word):Boolean;
var
 Transport:TAdapterTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter: RemoveTransport (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Packet = ' + PacketTypeToString(APacketType));
  {$ENDIF}
  
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;
  
  {Get Transport}
  Transport:=TAdapterTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Transport = nil then Exit;
  
  {Check Transport}
  if Transport.PacketType <> APacketType then
   begin
    {Unlock Transport}
    Transport.WriterUnlock;
    Exit;
   end; 
  
  {Acquire Lock}
  FTransports.WriterLock;
  try
   {Remove Transport}
   FTransports.Remove(Transport);
  
   {Unlock Transport}
   Transport.WriterUnlock;
  
   {Destroy Transport}
   Transport.Free;
 
   {Return Result}
   Result:=True;
  finally
   {Release Lock}
   FTransports.WriterUnlock;
  end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TWiFiAdapter.GetMTU(AHandle:THandle):Word;
var
 Value:LongWord;
begin
 {}
 ReaderLock;
 try
  Result:=0;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter: GetMTU (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Handle = ' + IntToHex(AHandle,8));
  {$ENDIF}
 
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;

  {Check Device}
  if FDevice = nil then Exit;

  {Get Device MTU}
  if FDevice.DeviceControl(FDevice,NETWORK_CONTROL_GET_MTU,0,Value) <> ERROR_SUCCESS then Exit; //To Do
 
  {Return Result}
  Result:=Value;
 finally 
  ReaderUnlock;
 end; 
end;
    
{==============================================================================}

function TWiFiAdapter.SendPacket(AHandle:THandle;ADest:Pointer;APacket:PPacketFragment;ASize:Integer):Boolean;
var
 Size:Integer;
 Length:LongWord;
 Buffer:Pointer;
 Packet:PPacketFragment;
 Ethernet:PEthernetHeader;
 Transport:TAdapterTransport;
begin
 {}
 Result:=False;
  
 {$IFDEF WIFI_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter: SendPacket (' + Name + ')');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Handle = ' + IntToHex(AHandle,8));
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Size = ' + IntToStr(ASize));
 {$ENDIF}
  
 {Check Dest}
 if ADest = nil then Exit;
  
 {Check Packet}
 if APacket = nil then Exit;
  
 {Check Status}
 if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;
 
 {Check Device}
 if FDevice = nil then Exit;
 
 {Get Transport}
 Transport:=TAdapterTransport(GetTransportByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Transport = nil then Exit;
 try
  //To Do
 finally 
  Transport.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TWiFiAdapter.GetDefaultAddress(AHandle:THandle):THardwareAddress; 
begin
 {}
 ReaderLock;
 try
  FillChar(Result,SizeOf(THardwareAddress),0);
 
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter: GetDefaultAddress (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Handle = ' + IntToHex(AHandle,8));
  {$ENDIF}
 
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;

  {Check Device}
  if FDevice = nil then Exit;
  
  {Return Result}
  Result:=HARDWARE_DEFAULT;
 finally 
  ReaderUnlock;
 end; 
end;
    
{==============================================================================}

function TWiFiAdapter.GetHardwareAddress(AHandle:THandle):THardwareAddress;
var
 Value:LongWord;
begin
 {}
 ReaderLock;
 try
  FillChar(Result,SizeOf(THardwareAddress),0);
 
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter: GetHardwareAddress (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Handle = ' + IntToHex(AHandle,8));
  {$ENDIF}
 
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;
  
  {Check Device}
  if FDevice = nil then Exit;
  
  {Get Hardware Address}
  FDevice.DeviceControl(FDevice,NETWORK_CONTROL_GET_HARDWARE,LongWord(@Result),Value);
 finally 
  ReaderUnlock;
 end; 
end;
    
{==============================================================================}

function TWiFiAdapter.SetHardwareAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean;
var
 Value:LongWord;
begin
 {}
 WriterLock;
 try
  Result:=False;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter: SetHardwareAddress (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Address = ' + HardwareAddressToString(AAddress));
  {$ENDIF}
  
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;
  
  {Check Device}
  if FDevice = nil then Exit;
  
  {Set Hardware Address}
  if FDevice.DeviceControl(FDevice,NETWORK_CONTROL_SET_MAC,LongWord(@AAddress),Value) = ERROR_SUCCESS then
   begin
    FHardwareAddress:=AAddress;
   
    {Return Result}
    Result:=True;
   end;
 finally 
  WriterUnlock;
 end; 
end;
    
{==============================================================================}

function TWiFiAdapter.GetBroadcastAddress(AHandle:THandle):THardwareAddress;
var
 Value:LongWord;
begin
 {}
 ReaderLock;
 try
  FillChar(Result,SizeOf(THardwareAddress),0);
 
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter: GetBroadcastAddress (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Handle = ' + IntToHex(AHandle,8));
  {$ENDIF}
 
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;

  {Check Device}
  if FDevice = nil then Exit;
  
  {Get Hardware Address}
  FDevice.DeviceControl(FDevice,NETWORK_CONTROL_GET_BROADCAST,LongWord(@Result),Value);
 finally 
  ReaderUnlock;
 end; 
end;
    
{==============================================================================}

function TWiFiAdapter.GetMulticastAddresses(AHandle:THandle):TMulticastAddresses;
begin
 {}
 ReaderLock;
 try
  FillChar(Result,SizeOf(TMulticastAddresses),0);
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter: GetMulticastAddresses (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Handle = ' + IntToHex(AHandle,8));
  {$ENDIF}
  
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;

  {Check Device}
  if FDevice = nil then Exit;
  
  //To Do
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TWiFiAdapter.AddMulticastAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean;
begin
 {}
 WriterLock;
 try
  Result:=False;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter: AddMulticastAddress (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Address = ' + HardwareAddressToString(AAddress));
  {$ENDIF}
  
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;

  {Check Device}
  if FDevice = nil then Exit;
  
  //To Do
 finally 
  WriterUnlock;
 end; 
end;

{==============================================================================}

function TWiFiAdapter.RemoveMulticastAddress(AHandle:THandle;const AAddress:THardwareAddress):Boolean;
begin
 {}
 WriterLock;
 try
  Result:=False;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter: RemoveMulticastAddress (' + Name + ')');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Handle = ' + IntToHex(AHandle,8));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter:  Address = ' + HardwareAddressToString(AAddress));
  {$ENDIF}
  
  {Check Status}
  if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;

  {Check Device}
  if FDevice = nil then Exit;
  
  //To Do
 finally 
  WriterUnlock;
 end; 
end;
    
{==============================================================================}

function TWiFiAdapter.StartAdapter:Boolean;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter: StartAdapter (' + Name + ')');
  {$ENDIF}
  
  {Check Status} 
  if FStatus <> ADAPTER_STATUS_UNKNOWN then Exit;
  
  {Check Device}
  if FDevice = nil then Exit;
    
  //To Do  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}
    
function TWiFiAdapter.StopAdapter:Boolean;
var
 ResultCode:LongWord;
 Current:TAdapterTransport;
 Transport:TAdapterTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
   
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFiAdapter: StopAdapter (' + Name + ')');
  {$ENDIF}

  {Check Status} 
  if FStatus <> ADAPTER_STATUS_READY then Exit;
  
  {Check Device}
  if FDevice = nil then Exit;
  
  {Check Thread}
  if FThread = nil then Exit;
    
  //To Do  
 finally 
  ReaderUnlock;
 end; 
end;
    
{==============================================================================}

function TWiFiAdapter.ProcessAdapter:Boolean;
begin
 {}
 Result:=False;
 
 {Check Status}
 if FStatus = ADAPTER_STATUS_UNKNOWN then Exit;

 {Check Device}
 if FDevice = nil then Exit;
 
 {Check Thread}
 if FThread = nil then Exit;

 //To Do
end;
 
{==============================================================================}
{==============================================================================}
{TEAPOLTransportAdapter}
constructor TEAPOLTransportAdapter.Create;
begin
 {}
 inherited Create;
end;
 
{==============================================================================}
{==============================================================================}
{TEAPOLTransport}
constructor TEAPOLTransport.Create(AManager:TTransportManager;const AName:String);
begin
 {}
 inherited Create(AManager,AName);
 FFamily:=AF_UNSPEC;
 FPacketType:=PACKET_TYPE_EAPOL;

 //To Do
end;

{==============================================================================}

destructor TEAPOLTransport.Destroy; 
begin
 {}
 WriterLock;
 try
  //To Do
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TEAPOLTransport.PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Process a packet received by an Adapter}
{Handle: The Handle of the Transport Adapter the packet was received from}
{Source: The source hardware address of the received packet (Set by Adapter)}
{Dest: The destination hardware address of the received packet (Set by Adapter)}
{Packet: The received packet (The complete packet without Adapter header)}
{Size: The size of the received packet in bytes}
{Broadcast: True if the destination address is a broadcast address}
var
 Adapter:TEAPOLTransportAdapter;
begin
 {}
 Result:=False;
 
 {$IFDEF WIFI_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL: PacketHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Adapter}
 Adapter:=TEAPOLTransportAdapter(GetAdapterByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL:  PacketType = ' + PacketTypeToString(Adapter.PacketType));
  {$ENDIF}
 
  {Check Packet Type}
  case Adapter.PacketType of
   PACKET_TYPE_EAPOL:begin
    
     //To Do

    end;
  end;
 finally 
  Adapter.ReaderUnlock;
 end; 
end;
 
{==============================================================================}

function TEAPOLTransport.AddAdapter(AAdapter:TNetworkAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean;
{Add an adapter to this transport}
{Adapter: The adapter to add}
{ConfigType: The configuration type to use for configuring the adapter (eg CONFIG_TYPE_AUTO)}
{Address: The transport address to use for this adapter (or nil if supplied during configuration)}
{Netmask: The transport netmask to use for this adapter (or nil if supplied during configuration)}
{Gateway: The transport default gateway to use for this adapter (or nil if supplied during configuration)}
{Server: The transport configuration server to use for this adapter (or nil if supplied during configuration)}
var
 Handle:THandle;
 Adapter:TEAPOLTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL: AddAdapter');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL:  Config = ' + ConfigTypeToString(AConfigType));
  {$ENDIF}
  
  {Check Adapter}
  if AAdapter = nil then Exit;
  
  {Check Status}
  if AAdapter.Status <> ADAPTER_STATUS_READY then Exit;
  
  {Get Adapter}
  Adapter:=TEAPOLTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
  if Adapter = nil then
   begin
    {Add EAPOL Type}
    Handle:=AAdapter.AddTransport(PACKET_TYPE_EAPOL,FRAME_TYPE_ETHERNET_II,EAPOL_TRANSPORT_NAME,PacketHandler);
    if Handle <> INVALID_HANDLE_VALUE then
     begin
      {Create Adapter}
      Adapter:=TEAPOLTransportAdapter.Create;
      Adapter.Name:=AAdapter.Name;
      Adapter.Handle:=Handle;
      Adapter.PacketType:=PACKET_TYPE_EAPOL;
      Adapter.Adapter:=AAdapter;
      Adapter.Hardware:=AAdapter.GetHardwareAddress(Handle);
      Adapter.Broadcast:=AAdapter.GetBroadcastAddress(Handle);
      Adapter.MTU:=AAdapter.GetMTU(Handle);
      Adapter.ConfigType:=CONFIG_TYPE_AUTO;
      Adapter.Configured:=True;
      Adapter.Configuring:=False;
      
      {Acquire Lock}
      FAdapters.WriterLock;
      try
       {Add Adapter}
       FAdapters.Add(Adapter);
      
       {Return Result}
       Result:=True;
      finally
       {Release Lock}
       FAdapters.WriterUnlock;
      end;  
     end;
   end
  else
   begin
    {Unlock Adapter}
    Adapter.ReaderUnlock;
    
    {Return Result}
    Result:=True;
   end;
 finally 
  ReaderUnlock;
 end; 
end;
 
{==============================================================================}

function TEAPOLTransport.RemoveAdapter(AAdapter:TNetworkAdapter):Boolean;
{Remove an adapter from this transport}
{Adapter: The adapter to remove}
var
 Adapter:TEAPOLTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL: RemoveAdapter');
  {$ENDIF}
  
  {Check Adapter}
  if AAdapter = nil then Exit;
 
  {Get Adapter}
  Adapter:=TEAPOLTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Adapter = nil then Exit;
  
  {Remove EAPOL Type}
  if AAdapter.RemoveTransport(Adapter.Handle,Adapter.PacketType) then
   begin
    {Acquire Lock}
    FAdapters.WriterLock;
    try
     {Remove Adapter}
     FAdapters.Remove(Adapter);
    
     {Unlock Adapter}
     Adapter.WriterUnlock;
    
     {Destroy Adapter}
     Adapter.Free;
     
     {Return Result}
     Result:=True;
    finally
     {Release Lock}
     FAdapters.WriterUnlock;
    end;  
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TEAPOLTransport.StartTransport:Boolean;
{Start this transport ready for sending and receiving}
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL: StartTransport');
  {$ENDIF}
  
  {Check Manager}
  if Manager = nil then Exit;
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TEAPOLTransport.StopTransport:Boolean;
{Stop this transport ready for removal}
var
 Current:TEAPOLTransportAdapter;
 Adapter:TEAPOLTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL: StopTransport');
  {$ENDIF}
  
  {Check Manager}
  if Manager = nil then Exit;
  
  {Get Adapter}
  Adapter:=TEAPOLTransportAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Adapter <> nil do
   begin
    {Get Next}
    Current:=Adapter;
    Adapter:=TEAPOLTransportAdapter(GetAdapterByNext(Current,True,True,NETWORK_LOCK_READ));
    
    {Remove Adapter} 
    RemoveAdapter(Adapter.Adapter);
   end;
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TEAPOLTransport.ProcessTransport:Boolean;
{Process periodic tasks for this transport}
begin
 {}
 //To Do
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TEAPOLTransport.BindTransport(AAdapter:TNetworkAdapter):Boolean; 
{Bind this transport to an adapter if appropriate}
{Adapter: The adapter to bind to}
var
 Adapter:TEAPOLTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False; 
 
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL: BindTransport');
  {$ENDIF}
 
  {Check Adapter}
  if AAdapter = nil then Exit;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL:  Adapter = ' + AAdapter.Name);
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL:   Status = ' + AdapterStatusToString(AAdapter.Status));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL:   Type = ' + AdapterTypeToString(AAdapter.AdapterType));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL:   Media = ' + MediaTypeToString(AAdapter.MediaType));
  {$ENDIF}
  
  {Check Status}
  if AAdapter.Status <> ADAPTER_STATUS_READY then Exit;
  
  Result:=True;
  
  {Check Media}
  if AAdapter.MediaType <> MEDIA_TYPE_ETHERNET then Exit;
  
  {Check Type}
  if AAdapter.AdapterType = ADAPTER_TYPE_UNKNOWN then Exit;
  
  Result:=False; 
  
  {Get Adapter}
  Adapter:=TEAPOLTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
  if Adapter = nil then
   begin
    {Check Type}
    case AAdapter.AdapterType of
     ADAPTER_TYPE_WIRELESS:begin 
       {Add Adapter}
       Result:=AddAdapter(AAdapter,CONFIG_TYPE_AUTO,nil,nil,nil,nil);
      end;     
    end;
   end
  else
   begin
    {Unlock Adapter}
    Adapter.ReaderUnlock;
    
    {Return Result}
    Result:=True;
   end;   
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TEAPOLTransport.UnbindTransport(AAdapter:TNetworkAdapter):Boolean; 
{Unbind this transport from an adapter if appropriate}
{Adapter: The adapter to unbind from}
var
 Adapter:TEAPOLTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False; 
 
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL: UnbindTransport');
  {$ENDIF}
 
  {Check Adapter}
  if AAdapter = nil then Exit;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'EAPOL:  Adapter = ' + AAdapter.Name);
  {$ENDIF}

  {Get Adapter}
  Adapter:=TEAPOLTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
  if Adapter <> nil then
   begin
    {Unlock Adapter}
    Adapter.ReaderUnlock;

    {Remove Adapter}
    Result:=RemoveAdapter(AAdapter);
   end
  else
   begin
    {Return Result}
    Result:=True;
   end;   
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TRSNTransportAdapter}
constructor TRSNTransportAdapter.Create;
begin
 {}
 inherited Create;
end;

{==============================================================================}
{==============================================================================}
{TRSNTransport}
constructor TRSNTransport.Create(AManager:TTransportManager;const AName:String);
begin
 {}
 inherited Create(AManager,AName);
 FFamily:=AF_UNSPEC;
 FPacketType:=PACKET_TYPE_RSN;

 //To Do
end;

{==============================================================================}

destructor TRSNTransport.Destroy; 
begin
 {}
 WriterLock;
 try
  //To Do
 finally
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TRSNTransport.PacketHandler(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
{Process a packet received by an Adapter}
{Handle: The Handle of the Transport Adapter the packet was received from}
{Source: The source hardware address of the received packet (Set by Adapter)}
{Dest: The destination hardware address of the received packet (Set by Adapter)}
{Packet: The received packet (The complete packet without Adapter header)}
{Size: The size of the received packet in bytes}
{Broadcast: True if the destination address is a broadcast address}
var
 Adapter:TRSNTransportAdapter;
begin
 {}
 Result:=False;
 
 {$IFDEF WIFI_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN: PacketHandler');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN:  Size = ' + IntToStr(ASize));
 {$ENDIF}
 
 {Get Adapter}
 Adapter:=TRSNTransportAdapter(GetAdapterByHandle(AHandle,True,NETWORK_LOCK_READ));
 if Adapter = nil then Exit;
 try
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN:  PacketType = ' + PacketTypeToString(Adapter.PacketType));
  {$ENDIF}
 
  {Check Packet Type}
  case Adapter.PacketType of
   PACKET_TYPE_RSN:begin
    
     //To Do

    end;
  end;
 finally 
  Adapter.ReaderUnlock;
 end; 
end;
 
{==============================================================================}

function TRSNTransport.AddAdapter(AAdapter:TNetworkAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean;
{Add an adapter to this transport}
{Adapter: The adapter to add}
{ConfigType: The configuration type to use for configuring the adapter (eg CONFIG_TYPE_AUTO)}
{Address: The transport address to use for this adapter (or nil if supplied during configuration)}
{Netmask: The transport netmask to use for this adapter (or nil if supplied during configuration)}
{Gateway: The transport default gateway to use for this adapter (or nil if supplied during configuration)}
{Server: The transport configuration server to use for this adapter (or nil if supplied during configuration)}
var
 Handle:THandle;
 Adapter:TRSNTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN: AddAdapter');
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN:  Config = ' + ConfigTypeToString(AConfigType));
  {$ENDIF}
  
  {Check Adapter}
  if AAdapter = nil then Exit;
  
  {Check Status}
  if AAdapter.Status <> ADAPTER_STATUS_READY then Exit;
  
  {Get Adapter}
  Adapter:=TRSNTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
  if Adapter = nil then
   begin
    {Add RSN Type}
    Handle:=AAdapter.AddTransport(PACKET_TYPE_RSN,FRAME_TYPE_ETHERNET_II,RSN_TRANSPORT_NAME,PacketHandler);
    if Handle <> INVALID_HANDLE_VALUE then
     begin
      {Create Adapter}
      Adapter:=TRSNTransportAdapter.Create;
      Adapter.Name:=AAdapter.Name;
      Adapter.Handle:=Handle;
      Adapter.PacketType:=PACKET_TYPE_RSN;
      Adapter.Adapter:=AAdapter;
      Adapter.Hardware:=AAdapter.GetHardwareAddress(Handle);
      Adapter.Broadcast:=AAdapter.GetBroadcastAddress(Handle);
      Adapter.MTU:=AAdapter.GetMTU(Handle);
      Adapter.ConfigType:=CONFIG_TYPE_AUTO;
      Adapter.Configured:=True;
      Adapter.Configuring:=False;
      
      {Acquire Lock}
      FAdapters.WriterLock;
      try
       {Add Adapter}
       FAdapters.Add(Adapter);
      
       {Return Result}
       Result:=True;
      finally
       {Release Lock}
       FAdapters.WriterUnlock;
      end;  
     end;
   end
  else
   begin
    {Unlock Adapter}
    Adapter.ReaderUnlock;
    
    {Return Result}
    Result:=True;
   end;
 finally 
  ReaderUnlock;
 end; 
end;
 
{==============================================================================}

function TRSNTransport.RemoveAdapter(AAdapter:TNetworkAdapter):Boolean;
{Remove an adapter from this transport}
{Adapter: The adapter to remove}
var
 Adapter:TRSNTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN: RemoveAdapter');
  {$ENDIF}
  
  {Check Adapter}
  if AAdapter = nil then Exit;
 
  {Get Adapter}
  Adapter:=TRSNTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_WRITE)); {Writer due to remove}
  if Adapter = nil then Exit;
  
  {Remove RSN Type}
  if AAdapter.RemoveTransport(Adapter.Handle,Adapter.PacketType) then
   begin
    {Acquire Lock}
    FAdapters.WriterLock;
    try
     {Remove Adapter}
     FAdapters.Remove(Adapter);
    
     {Unlock Adapter}
     Adapter.WriterUnlock;
    
     {Destroy Adapter}
     Adapter.Free;
     
     {Return Result}
     Result:=True;
    finally
     {Release Lock}
     FAdapters.WriterUnlock;
    end;  
   end;
 finally 
  ReaderUnlock;
 end; 
end;
 
{==============================================================================}

function TRSNTransport.StartTransport:Boolean;
{Start this transport ready for sending and receiving}
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN: StartTransport');
  {$ENDIF}
  
  {Check Manager}
  if Manager = nil then Exit;
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TRSNTransport.StopTransport:Boolean;
{Stop this transport ready for removal}
var
 Current:TRSNTransportAdapter;
 Adapter:TRSNTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN: StopTransport');
  {$ENDIF}
  
  {Check Manager}
  if Manager = nil then Exit;
  
  {Get Adapter}
  Adapter:=TRSNTransportAdapter(GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Adapter <> nil do
   begin
    {Get Next}
    Current:=Adapter;
    Adapter:=TRSNTransportAdapter(GetAdapterByNext(Current,True,True,NETWORK_LOCK_READ));
    
    {Remove Adapter} 
    RemoveAdapter(Adapter.Adapter);
   end;
  
  {Return Result}
  Result:=True;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TRSNTransport.ProcessTransport:Boolean;
{Process periodic tasks for this transport}
begin
 {}
 //To Do
 
 {Return Result}
 Result:=True;
end;

{==============================================================================}

function TRSNTransport.BindTransport(AAdapter:TNetworkAdapter):Boolean; 
{Bind this transport to an adapter if appropriate}
{Adapter: The adapter to bind to}
var
 Adapter:TRSNTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False; 
 
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN: BindTransport');
  {$ENDIF}
 
  {Check Adapter}
  if AAdapter = nil then Exit;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN:  Adapter = ' + AAdapter.Name);
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN:   Status = ' + AdapterStatusToString(AAdapter.Status));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN:   Type = ' + AdapterTypeToString(AAdapter.AdapterType));
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN:   Media = ' + MediaTypeToString(AAdapter.MediaType));
  {$ENDIF}
  
  {Check Status}
  if AAdapter.Status <> ADAPTER_STATUS_READY then Exit;
  
  Result:=True;
  
  {Check Media}
  if AAdapter.MediaType <> MEDIA_TYPE_ETHERNET then Exit;
  
  {Check Type}
  if AAdapter.AdapterType = ADAPTER_TYPE_UNKNOWN then Exit;
  
  Result:=False; 
  
  {Get Adapter}
  Adapter:=TRSNTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
  if Adapter = nil then
   begin
    {Check Type}
    case AAdapter.AdapterType of
     ADAPTER_TYPE_WIRELESS:begin 
       {Add Adapter}
       Result:=AddAdapter(AAdapter,CONFIG_TYPE_AUTO,nil,nil,nil,nil);
      end;     
    end;
   end
  else
   begin
    {Unlock Adapter}
    Adapter.ReaderUnlock;
    
    {Return Result}
    Result:=True;
   end;   
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TRSNTransport.UnbindTransport(AAdapter:TNetworkAdapter):Boolean; 
{Unbind this transport from an adapter if appropriate}
{Adapter: The adapter to unbind from}
var
 Adapter:TRSNTransportAdapter;
begin
 {}
 ReaderLock;
 try
  Result:=False; 
 
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN: UnbindTransport');
  {$ENDIF}
 
  {Check Adapter}
  if AAdapter = nil then Exit;
  
  {$IFDEF WIFI_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RSN:  Adapter = ' + AAdapter.Name);
  {$ENDIF}

  {Get Adapter}
  Adapter:=TRSNTransportAdapter(GetAdapterByAdapter(AAdapter,True,NETWORK_LOCK_READ));
  if Adapter <> nil then
   begin
    {Unlock Adapter}
    Adapter.ReaderUnlock;

    {Remove Adapter}
    Result:=RemoveAdapter(AAdapter);
   end
  else
   begin
    {Return Result}
    Result:=True;
   end;   
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure WiFiInit;
begin
 {}
 {Check Initialized}
 if WiFiInitialized then Exit;
 
 {Create EAPOL Transport}
 if EAPOL_TRANSPORT_ENABLED then
  begin
   TEAPOLTransport.Create(TransportManager,EAPOL_TRANSPORT_NAME);
  end; 
 
 {Create RSN Transport}
 if RSN_TRANSPORT_ENABLED then
  begin
   TRSNTransport.Create(TransportManager,RSN_TRANSPORT_NAME);
  end; 
 
 {Register Start Event}
 NetworkEventRegister(WiFiStart,nil,NETWORK_EVENT_SYSTEM_START);
 
 {Register Stop Event}
 NetworkEventRegister(WiFiStop,nil,NETWORK_EVENT_SYSTEM_STOP);
 
 WiFiInitialized:=True;
end;

{==============================================================================}

function WiFiStart(Data:Pointer;Event:LongWord):LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if WiFiStarted then Exit;
 
 {Check Event}
 if (Event and NETWORK_EVENT_SYSTEM_START) <> 0 then
  begin
   Result:=ERROR_INVALID_PARAMETER;
   
   {Enumerate Adapters}
   NetworkDeviceEnumerate(WiFiNetworkDeviceEnum,nil);
   
   {Register Notification}
   NetworkDeviceNotification(nil,WiFiNetworkDeviceNotify,nil,DEVICE_NOTIFICATION_REGISTER or DEVICE_NOTIFICATION_DEREGISTER or DEVICE_NOTIFICATION_CLOSING,NOTIFIER_FLAG_NONE);
   
   {Set Started} 
   WiFiStarted:=True;
   
   {Return Result} 
   Result:=ERROR_SUCCESS;
  end; 
end;

{==============================================================================}

function WiFiStop(Data:Pointer;Event:LongWord):LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if not(WiFiStarted) then Exit;
 
 {Check Event}
 if (Event and NETWORK_EVENT_SYSTEM_STOP) <> 0 then
  begin
   Result:=ERROR_INVALID_PARAMETER;
   
   {Deregister Notification}
   NetworkDeviceNotification(nil,WiFiNetworkDeviceNotify,nil,DEVICE_NOTIFICATION_NONE,NOTIFIER_FLAG_NONE);
   
   {Set Started}
   WiFiStarted:=False;    
   
   {Return Result} 
   Result:=ERROR_SUCCESS;
  end; 
end;
 
{==============================================================================}
 
function WiFiStartCompleted:Boolean;
{Returns True if the WiFi sub system has been started}
begin
 {}
 Result:=WiFiStarted;
end;
 
{==============================================================================}
{==============================================================================}
{WiFi Functions}
function WiFiDeviceStart(WiFi:PWiFiDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check WiFi}
 if WiFi = nil then Exit;
 if WiFi.Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 if not Assigned(WiFi.DeviceStart) then Exit;
 
 {Call Start}
 Result:=WiFi.DeviceStart(WiFi);
end;

{==============================================================================}

function WiFiDeviceStop(WiFi:PWiFiDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check WiFi}
 if WiFi = nil then Exit;
 if WiFi.Network.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 if not Assigned(WiFi.DeviceStop) then Exit;
 
 {Call Stop}
 Result:=WiFi.DeviceStop(WiFi);
end;

{==============================================================================}
{==============================================================================}
{WiFi Helper Functions}
procedure WiFiNetworkDeviceAdd(Event:PWiFiDeviceEvent);
var
 Adapter:TNetworkAdapter;
begin
 {}
 {$IFDEF WIFI_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFi: Network device add');
 {$ENDIF}
 
 {Check Event}
 if Event = nil then Exit;
 if Event.Device = nil then Exit;
 
 {$IFDEF WIFI_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFi:  Device = ' + DeviceGetName(@Event.Device.Device));
 {$ENDIF}
 
 {Destroy Timer}
 if Event.Timer <> INVALID_HANDLE_VALUE then
  begin
   TimerDestroy(Event.Timer);
   Event.Timer:=INVALID_HANDLE_VALUE;
  end;

 {Check Managers}
 if AdapterManager = nil then Exit;
 if TransportManager = nil then Exit;

 {Check Started}
 if WiFiStarted then
  begin
   {Check Type}
   case Event.Device.Device.DeviceType of
    NETWORK_TYPE_80211:begin
      {Check Adapter}
      Adapter:=AdapterManager.GetAdapterByDevice(Event.Device,False,NETWORK_LOCK_NONE); {Do not lock}
      if Adapter = nil then
       begin
        {Create Adapter}
        if WIRELESS_NETWORK_ENABLED then
         begin
          Adapter:=TWiFiAdapter.Create(AdapterManager,Event.Device,DeviceGetName(@Event.Device.Device));
      
          {Start Adapter}
          Adapter.StartAdapter;
      
          {Bind Transports}
          TransportManager.BindTransports(Adapter);
         end; 
       end; 
     end;
   end; 
  end;  
end;

{==============================================================================}

function WiFiNetworkDeviceRemove(Network:PNetworkDevice):LongWord;
var
 Adapter:TNetworkAdapter;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF WIFI_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFi: Network device remove');
 {$ENDIF}
 
 {Check Network}
 if Network = nil then Exit;

 {$IFDEF WIFI_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFi:  Device = ' + DeviceGetName(@Network.Device));
 {$ENDIF}
 
 {Check Managers}
 if AdapterManager = nil then Exit;
 if TransportManager = nil then Exit;

 {Check Started}
 if WiFiStarted then
  begin
   {Check Type}
   case Network.Device.DeviceType of
    NETWORK_TYPE_80211:begin
      {Check Adapter}
      Adapter:=AdapterManager.GetAdapterByDevice(Network,True,NETWORK_LOCK_READ);
      if Adapter <> nil then
       begin
        {Unbind Transports}
        TransportManager.UnbindTransports(Adapter);
        
        {Stop Adapter}
        Adapter.StopAdapter;
        
        {Unlock Adapter}
        Adapter.ReaderUnlock;
        
        {Free Adapter}
        Adapter.Free;
       end; 
     end;
   end; 
  end;  
  
 {Return Result}
 Result:=ERROR_SUCCESS;    
end;

{==============================================================================}

function WiFiNetworkDeviceEnum(Network:PNetworkDevice;Data:Pointer):LongWord;
var
 Adapter:TNetworkAdapter;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {$IFDEF WIFI_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFi: Network device enumeration');
 {$ENDIF}

 {Check Network}
 if Network = nil then Exit;
 
 {$IFDEF WIFI_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFi:  Device = ' + DeviceGetName(@Network.Device));
 {$ENDIF}
 
 {Check Manager}
 if AdapterManager = nil then Exit;

 {Check Started}
 if not WiFiStarted then
  begin
   {Check Type}
   case Network.Device.DeviceType of
    NETWORK_TYPE_80211:begin
      {Check Adapter}
      Adapter:=AdapterManager.GetAdapterByDevice(Network,False,NETWORK_LOCK_NONE); {Do not lock}
      if Adapter = nil then
       begin
        {Create Adapter}
        if WIRELESS_NETWORK_ENABLED then
         begin
          TWiFiAdapter.Create(AdapterManager,Network,DeviceGetName(@Network.Device));
         end; 
       end; 
     end;
   end; 
  end;  
end;
 
{==============================================================================}

function WiFiNetworkDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
var
 Adapter:TNetworkAdapter;
 Event:PWiFiDeviceEvent;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {$IFDEF WIFI_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'WiFi: Network device notification (Notification=' + NotificationToString(Notification) + ')');
 {$ENDIF}
 
 {Check Device}
 if Device = nil then Exit;
 
 {Check Manager}
 if AdapterManager = nil then Exit;

 {Check Notification}
 if (Notification and DEVICE_NOTIFICATION_REGISTER) <> 0 then
  begin
   {Check Started}
   if WiFiStarted then
    begin
     {Check Adapter}
     Adapter:=AdapterManager.GetAdapterByDevice(PNetworkDevice(Device),False,NETWORK_LOCK_NONE); {Do not lock}
     if Adapter = nil then
      begin
       {Create Event}
       Event:=AllocMem(SizeOf(TWiFiDeviceEvent));
       if Event = nil then Exit;
       
       {Setup Event}
       Event.Timer:=INVALID_HANDLE_VALUE;
       Event.Device:=PNetworkDevice(Device);
       
       {Create Timer}
       Event.Timer:=TimerCreateEx(WIFI_DEVICE_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(WiFiNetworkDeviceAdd),Event);
       if Event.Timer = INVALID_HANDLE_VALUE then
        begin
         {Destroy Event}
         FreeMem(Event);
        end;
      end;
    end;
  end
 else if (Notification and DEVICE_NOTIFICATION_CLOSING) <> 0 then
  begin
   {Check Started}
   if WiFiStarted then
    begin
     {Check Adapter}
     Adapter:=AdapterManager.GetAdapterByDevice(PNetworkDevice(Device),False,NETWORK_LOCK_NONE); {Do not lock}
     if Adapter <> nil then
      begin
       {Remove Adapter}
       WiFiNetworkDeviceRemove(PNetworkDevice(Device));
      end;
    end;
  end
 else if (Notification and DEVICE_NOTIFICATION_DEREGISTER) <> 0 then
  begin
   {Check Started}
   if WiFiStarted then
    begin
     {Check Adapter}
     Adapter:=AdapterManager.GetAdapterByDevice(PNetworkDevice(Device),False,NETWORK_LOCK_NONE); {Do not lock}
     if Adapter <> nil then
      begin
       {Remove Adapter}
       WiFiNetworkDeviceRemove(PNetworkDevice(Device));
      end;
    end;
  end;
end;
 
{==============================================================================}
{==============================================================================}

initialization
 WiFiInit;
 if NetworkStartCompleted then
  begin
   WiFiStart(nil,NETWORK_EVENT_SYSTEM_START);
  end;
  
{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
