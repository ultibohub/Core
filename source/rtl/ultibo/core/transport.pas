{
Ultibo Network Transport interface unit.

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

 
Network Transport
=================

 Notes: Includes all global transport definitions plus base class
        for Network transports

        Currently supported transports are IP, ARP, RARP, IPX and
        IP6

 Notes: All HostToNetwork swaps occur at the level where they are
        appropriate (ie the level of their header)

 Notes: Bindings is provided to support Transports (such as IP) which
        can provide multiple addresses on a single adapter
        It is not used by the Protocol or Winsock layer and
        is only used by the transport where appropriate.
        In such cases the ConfigType in the Adapter object is
        not used and the Binding is used instead.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Transport;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,Threads,Devices,SysUtils,Classes,Network,Ultibo,UltiboClasses;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
const
 {Transport specific constants}
 HOST_TYPE_DYNAMIC   = 0;
 HOST_TYPE_STATIC    = 1;
 HOST_TYPE_LOOPBACK  = 2;

 ROUTE_TYPE_DYNAMIC   = 0;
 ROUTE_TYPE_STATIC    = 1;
 ROUTE_TYPE_LOOPBACK  = 2;  {Loopback Route}
 ROUTE_TYPE_GATEWAY   = 3;  {Default Route}
 ROUTE_TYPE_MULTICAST = 4;  {Multicast Route}
 ROUTE_TYPE_BROADCAST = 5;  {Broadcast Route}

 ADDRESS_TYPE_DYNAMIC   = 0;  {Remote Dynamic Address}
 ADDRESS_TYPE_STATIC    = 1;  {Remote Static Address}
 ADDRESS_TYPE_LOCAL     = 2;  {Local Address (ARP/RARP Only)}
 ADDRESS_TYPE_LOOPBACK  = 3;  {Local Loopback}
 ADDRESS_TYPE_MULTICAST = 4;  {Local Multicast}
 ADDRESS_TYPE_BROADCAST = 5;  {Local Broadcast}
 //To do //LinkLocal/SiteLocal/V4Mapped/MulticastNodeLocal/MulticastLinkLocal/MulticastSiteLocal/MulticastOrgLocal/MulticastGlobal //For IPv6
 
 ADDRESS_TYPE_PRIMARY   = 6;  {Same as Local but used by IP/IP6}
 ADDRESS_TYPE_SECONDARY = 7;  {Same as Local but used by IP/IP6}

const
 {Generic Transport}
 {Host Constants}
 MAX_HOST_LIFE = 600000;     {10 Minute Host lifespan}

 {Route Constants}
 MAX_ROUTE_LIFE = 600000;    {10 min Route lifespan }

 {Address Constants}
 MAX_ADDRESS_LIFE = 300000;  {5 min Address (ARP) lifespan }

 {IP Transport}

 {IP4 Transport}
 IP_ADDRESS_SIZE = 4;        {SizeOf(TInAddr)}

 {IP6 Transport}
 IP6_ADDRESS_SIZE = 16;      {SizeOf(TIn6Addr)}

 {ARP Transport}

 {RARP Transport}

 {IPX Transport}
 IPX_ADDRESS_SIZE = 12;      {SizeOf(TIpxAddr)}

const
 {Transport Sockets}
 TTL_DEFAULT = 128;        {Default to 128 Seconds/Hops (Defined as 64)}
 TOS_DEFAULT = 0;
 HOPLIMIT_DEFAULT = 255;   {Default to 255 Hops (Defined as 64)}
 ID_INCREMENT = 1;         {Default Increment for ID Numbers}
 TTL_DECREMENT = 1;        {Default Decrement for TTL when Forwarding}
 SEND_TIMEOUT = 0;         {Default to wait forever on Send}
 RECV_TIMEOUT = 0;         {Default to wait forever on Recv}
 CLOSE_TIMEOUT = 3000;     {Time that Socket is kept before destruction}
 LINGER_TIMEOUT = 60000;   {Time that Socket goes into Linger state for}
 CONNECT_TIMEOUT = 5000;   {Time that Socket waits for connect completion}
 TIMEWAIT_TIMEOUT = 240000;{Time that Socket goes into TimeWait state for (2 x MSL)}

 {Shutdown Constants}
 SHUTDOWN_RECV = 0;
 SHUTDOWN_SEND = 1;
 SHUTDOWN_BOTH = 2;

 {Select Constants}
 SELECT_READ = 0;
 SELECT_WRITE = 1;
 SELECT_ERROR = 2;

 {Socket Options}
 {See Sockets}

 {Socket States}
 SS_NOFDREF         = $0001;          // no file table ref any more
 SS_UNCONNECTED     = SS_NOFDREF;     //    or just created socket
 SS_ISCONNECTED     = $0002;          // socket connected to a peer
 SS_ISCONNECTING    = $0004;          // in process of connecting
 SS_ISDISCONNECTING = $0008;          // in process of disconnecting
 SS_CANTSENDMORE    = $0010;          // can't send more data
 SS_CANTRCVMORE     = $0020;          // can't receive more data
 SS_RCVATMARK       = $0040;          // at mark on input

 SS_PRIV            = $0080;          // privileged for broadcast
 SS_NBIO            = $0100;          // non-blocking ops
 SS_ASYNC           = $0200;          // async i/o notify
 SS_ISCONFIRMING    = $0400;          // accepting connection req
 SS_ISLISTENING     = SS_ISCONFIRMING; // non standard

 SS_LOCAL_ADDR      = $0800;          // has local address/port (not used)
 SS_REMOTE_ADDR     = $1000;          // has remote address/port (not used)
 SS_CONN_REFUSED    = $2000;          // connection refused (ICMP_UNREACH)
 SS_CLOSED          = $4000;          // socket has been closed
 
{==============================================================================}
type
 {Transport specific types}
 {Generic Transport}
 PTransportStatistics = ^TTransportStatistics;
 TTransportStatistics = record
  PacketsIn:Int64;
  PacketsOut:Int64;
  BytesIn:Int64; 
  BytesOut:Int64;
  ErrorsIn:Int64;
  ErrorsOut:Int64;
  PacketsLost:Int64;
 end;

 {IP Transport}

 {IP4 Transport}
 PIPNameservers = ^TIPNameservers;
 TIPNameservers = array[0..MAX_NAME_SERVERS - 1] of TInAddr;

 {IP6 Transport}
 PIP6Nameservers = ^TIP6Nameservers;
 TIP6Nameservers = array[0..MAX_NAME_SERVERS - 1] of TIn6Addr;

 {ARP Transport}

 {RARP Transport}

 {IPX Transport}

{type}
 {Transport Sockets}
 
{==============================================================================}
type
 {Transport specific classes}
 TNetworkTransport = class;
 TTransportCallback = function(ATransport:TNetworkTransport):Boolean of object;
  
 TNetworkMonitor = class;
 TMonitorCallback = function(AMonitor:TNetworkMonitor):Boolean of object;
 
 TNetworkAuthenticator = class;
 TAuthenticatorCallback = function(AAuthenticator:TNetworkAuthenticator):Boolean of object;
 
 TTransportManager = class(TObject)
   constructor Create(ASettings:TNetworkSettings;AAdapters:TAdapterManager);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
   FSettings:TNetworkSettings;
   FAdapters:TAdapterManager;
   
   {Status Variables}
   FTransports:TNetworkList;     {List of TNetworkTransport objects}
   FMonitors:TNetworkList;       {List of TNetworkMonitor objects}
   FAuthenticators:TNetworkList; {List of TNetworkAuthenticator objects}
   
   {Event Variables}
   
   {Internal Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
  public
   {Public Properties}
   property Settings:TNetworkSettings read FSettings;
   property Adapters:TAdapterManager read FAdapters;
   
   {Public Methods}
   function AddTransport(ATransport:TNetworkTransport):Boolean;
   function RemoveTransport(ATransport:TNetworkTransport):Boolean;
   
   function GetTransportByType(AFamily,APacketType:Word;ALock:Boolean;AState:LongWord):TNetworkTransport;
   function GetTransportByTransport(ATransport:TNetworkTransport;ALock:Boolean;AState:LongWord):TNetworkTransport;
   function GetTransportByNext(APrevious:TNetworkTransport;ALock,AUnlock:Boolean;AState:LongWord):TNetworkTransport;
   
   function AddMonitor(AMonitor:TNetworkMonitor):Boolean;
   function RemoveMonitor(AMonitor:TNetworkMonitor):Boolean;
   
   function GetMonitorByMonitor(AMonitor:TNetworkMonitor;ALock:Boolean;AState:LongWord):TNetworkMonitor;
   function GetMonitorByNext(APrevious:TNetworkMonitor;ALock,AUnlock:Boolean;AState:LongWord):TNetworkMonitor;
   
   function AddAuthenticator(AAuthenticator:TNetworkAuthenticator):Boolean;
   function RemoveAuthenticator(AAuthenticator:TNetworkAuthenticator):Boolean;
   
   function GetAuthenticatorByType(AAuthType:Word;ALock:Boolean;AState:LongWord):TNetworkAuthenticator;
   function GetAuthenticatorByAuthenticator(AAuthenticator:TNetworkAuthenticator;ALock:Boolean;AState:LongWord):TNetworkAuthenticator;
   function GetAuthenticatorByNext(APrevious:TNetworkAuthenticator;ALock,AUnlock:Boolean;AState:LongWord):TNetworkAuthenticator;
   
   function StartTransports:Boolean;
   function StopTransports:Boolean;
   function ProcessTransports:Boolean;
   
   function EnumerateTransports(ACallback:TTransportCallback):Boolean;
   
   function BindTransports(AAdapter:TNetworkAdapter):Boolean;
   function UnbindTransports(AAdapter:TNetworkAdapter):Boolean;
   
   function StartMonitors:Boolean;
   function StopMonitors:Boolean;
   function ProcessMonitors:Boolean;
   
   function EnumerateMonitors(ACallback:TMonitorCallback):Boolean;
   
   function BindMonitors(AAdapter:TNetworkAdapter):Boolean;
   function UnbindMonitors(AAdapter:TNetworkAdapter):Boolean;
   
   function StartAuthenticators:Boolean;
   function StopAuthenticators:Boolean;
   function ProcessAuthenticators:Boolean;
   
   function EnumerateAuthenticators(ACallback:TAuthenticatorCallback):Boolean;
   
   function BindAuthenticators(AAdapter:TNetworkAdapter):Boolean;
   function UnbindAuthenticators(AAdapter:TNetworkAdapter):Boolean;
 end;

 TTransportAdapter = class;
 TTransportPacketHandler = function(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean of object;
 TTransportControlHandler = function(AHandle:THandle;ASource,ADest:Pointer;AProtocol,ACommand,ACode:Word;AAddress,AData:Pointer;ASize:Integer):Boolean of object;
 TTransportFilterHandler = function(AHandle:THandle;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean of object;
 TTransportConfigHandler = function(AHandle:THandle;AAdapter:TTransportAdapter;ACommand:Word):Boolean of object;

 TTransportBuffer = class(TObject)
   constructor Create(ATransport:TNetworkTransport);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TCriticalSectionHandle;
  protected
   {Internal Variables}
   FTransport:TNetworkTransport;

   {Status Variables}

   {Event Methods}
   
   {Internal Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
  public
   {Status Variables}
   
 end;

 TTransportBufferEx = class(TObject)
   constructor Create(ATransport:TNetworkTransport);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  protected
   {Internal Variables}
   FTransport:TNetworkTransport;

   {Status Variables}

   {Event Methods}
   
   {Internal Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function ReaderConvert:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   function WriterConvert:Boolean;
   function WriterOwner:Boolean;
  public
   {Status Variables}
   
 end;
 
 TTransportAdapter = class(TListObject) {Downstream}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   Name:String;
   Index:LongWord;              //To do //Do we need this ? //Nothing seems to use it ? //Winsock uses it for IP Helper API
   Handle:THandle;              //To Do //Do these need lock protection ?
   PacketType:Word;
   Adapter:TNetworkAdapter;
   Hardware:THardwareAddress;
   Broadcast:THardwareAddress;

   MTU:Word;

   ConfigType:Word;     {BOOTP/DHCP/RARP/STATIC/PSEUDO/LOOPBACK}
   Configured:Boolean;  {Adapter has been configured}
   Configuring:Boolean; {Adapter is being configured}
   
   //To Do //Add RetryCount/RetryTimeout ?
   
   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;

 TTransportBinding = class(TListObject) {Midstream}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   Name:String;                      //To Do //Does this need lock protection and UniqueString ? 
   Index:LongWord;                   //To do //Do we need this ? //Nothing seems to use it ? //Winsock uses it for IP Helper API
   Adapter:TTransportAdapter;        //To Do //Do these need lock protection ?

   ConfigType:Word;     {BOOTP/DHCP/RARP/STATIC/PSEUDO/LOOPBACK}
   Configured:Boolean;  {Adapter has been configured}
   Configuring:Boolean; {Adapter is being configured}
   
   //To Do //Add RetryCount/RetryTimeout ?
   
   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;

 //To Do //TTransportTransport = class(TListObject) {Midstream} 
 //To allow transports to reference each other (eg IP needs ARP/RARP ?)
 
 TTransportProtocol = class(TListObject) {Upstream}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   Protocol:Word;                            //To Do //Do these need lock protection ?
   PacketHandler:TTransportPacketHandler;
   ControlHandler:TTransportControlHandler;
   
   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;

 TTransportFilter = class(TListObject) {Upstream}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   Protocol:Word;                            //To Do //Do these need lock protection ?
   FilterHandler:TTransportFilterHandler;
   
   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;

 TTransportConfig = class(TListObject) {Upstream}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   ConfigType:Word;    {BOOTP/DHCP/RARP/STATIC/PSEUDO/LOOPBACK}  //To Do //Do these need lock protection ?
   ConfigAuto:Boolean; {Supports Auto Configuration}
   //To Do //Add RetryCount/RetryTimeout - Yes
   ConfigHandler:TTransportConfigHandler;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;

 TTransportSocket = class;
 TNetworkTransport = class(TListObject)
   constructor Create(AManager:TTransportManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  protected
   {Internal Variables}
   FManager:TTransportManager;
   FName:String;
   
   {Status Variables}
   FFamily:Word;
   FPacketType:Word;
   FAdapters:TNetworkList;   {List of TTransportAdapter objects}
   FBindings:TNetworkList;   {List of TTransportBinding objects}
   FProtocols:TNetworkList;  {List of TTransportProtocol objects}
   FFilters:TNetworkList;    {List of TTransportFilter objects}
   FConfigs:TNetworkList;    {List of TTransportConfig objects}

   FStatistics:TTransportStatistics;

   {Event Methods}

   {Internal Methods}
   function GetAdapterByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TTransportAdapter;
   function GetAdapterByAdapter(AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TTransportAdapter;
   function GetAdapterByNext(APrevious:TTransportAdapter;ALock,AUnlock:Boolean;AState:LongWord):TTransportAdapter;
   
   //function GetBindingByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TTransportBinding; //To Do
   //function GetBindingByAdapter(AAdapter:TTransportAdapter;ALock:Boolean;AState:LongWord):TTransportBinding; //To Do
   //function GetBindingByBinding(ABinding:TTransportBinding;ALock:Boolean;AState:LongWord):TTransportBinding; //To Do
   //function GetBindingByNext(APrevious:TTransportBinding;ALock,AUnlock:Boolean;AState:LongWord):TTransportBinding; //To Do
   
   function GetProtocolByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TTransportProtocol;
   function GetProtocolByType(AProtocol:Word;ALock:Boolean;AState:LongWord):TTransportProtocol;
   function GetProtocolByProtocol(AProtocol:TTransportProtocol;ALock:Boolean;AState:LongWord):TTransportProtocol;
   function GetProtocolByNext(APrevious:TTransportProtocol;ALock,AUnlock:Boolean;AState:LongWord):TTransportProtocol;
   
   function GetFilterByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TTransportFilter;
   function GetFilterByProtocol(AProtocol:Word;ALock:Boolean;AState:LongWord):TTransportFilter; 
   function GetFilterByFilter(AFilter:TTransportFilter;ALock:Boolean;AState:LongWord):TTransportFilter;
   function GetFilterByNext(APrevious:TTransportFilter;ALock,AUnlock:Boolean;AState:LongWord):TTransportFilter;
   
   function GetConfigByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TTransportConfig;
   function GetConfigByType(AConfigType:Word;ALock:Boolean;AState:LongWord):TTransportConfig;
   function GetConfigByConfig(AConfig:TTransportConfig;ALock:Boolean;AState:LongWord):TTransportConfig;
   function GetConfigByNext(APrevious:TTransportConfig;ALock,AUnlock:Boolean;AState:LongWord):TTransportConfig;
   
   {Protected Methods}
   function AddAdapter(AAdapter:TNetworkAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean; virtual;
   function RemoveAdapter(AAdapter:TNetworkAdapter):Boolean; virtual;

   //function AddBinding(AAdapter:TTransportAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean; virtual; //To Do
   //function RemoveBinding(ABinding:TTransportBinding):Boolean; virtual; //To Do

   function AddProtocol(AProtocol:Word;APacketHandler:TTransportPacketHandler;AControlHandler:TTransportControlHandler):THandle; virtual;
   function RemoveProtocol(AHandle:THandle;AProtocol:Word):Boolean; virtual;

   function AddFilter(AProtocol:Word;AFilterHandler:TTransportFilterHandler):THandle; virtual;
   function RemoveFilter(AHandle:THandle;AProtocol:Word):Boolean; virtual;

   function AddConfig(AConfigType:Word;AConfigAuto:Boolean;AConfigHandler:TTransportConfigHandler):THandle; virtual;
   function RemoveConfig(AHandle:THandle;AConfigType:Word):Boolean; virtual;

   function SendPacket(ASocket:TTransportSocket;ASource,ADest:Pointer;APacket:PPacketFragment;ASize,AFlags:Integer):Integer; virtual;
   function SendControl(ASource,ADest:Pointer;AProtocol,ACommand,ACode:Word;AAddress,AData:Pointer;ASize:Integer):Boolean; virtual;

   function FilterPacket(ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean; virtual;
   function ForwardPacket(AAdapter:TTransportAdapter;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean; virtual;
  public
   {Public Properties}
   property Manager:TTransportManager read FManager;
   property Name:String read FName;   //To Do //Does this need lock protection and UniqueString ? //Yes //LocalLock(Mutex)
   
   property Family:Word read FFamily;
   property PacketType:Word read FPacketType;

   {BSD Socket Methods}
   function GetSockOpt(ASocket:TTransportSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer; virtual;
   function SetSockOpt(ASocket:TTransportSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer; virtual;

   function GetStatistics:TTransportStatistics; virtual;
    
   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   
   function StartTransport:Boolean; virtual;
   function StopTransport:Boolean; virtual;
   function ProcessTransport:Boolean; virtual;
   
   function BindTransport(AAdapter:TNetworkAdapter):Boolean; virtual;
   function UnbindTransport(AAdapter:TNetworkAdapter):Boolean; virtual;
 end;

 TMonitorAdapter = class(TListObject) {Downstream}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   Handle:THandle;                //To Do //Do these need lock protection ?
   Adapter:TNetworkAdapter;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;
 
 TNetworkMonitor = class(TListObject) {eg Packet Capture}
   constructor Create(AManager:TTransportManager);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  protected
   {Internal Variables}
   FManager:TTransportManager;
 
   {Status Variables}
   FAdapters:TNetworkList;   {List of TMonitorAdapter objects}

   {Event Methods}

   {Internal Methods}
   function GetAdapterByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TMonitorAdapter;
   function GetAdapterByAdapter(AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TMonitorAdapter;
   function GetAdapterByNext(APrevious:TMonitorAdapter;ALock,AUnlock:Boolean;AState:LongWord):TMonitorAdapter;

   {Protected Methods}
   function AddAdapter(AAdapter:TNetworkAdapter):Boolean; virtual;
   function RemoveAdapter(AAdapter:TNetworkAdapter):Boolean; virtual;
  public
   {Public Properties}
   property Manager:TTransportManager read FManager;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   
   function StartMonitor:Boolean; virtual;
   function StopMonitor:Boolean; virtual;
   function ProcessMonitor:Boolean; virtual;

   function BindMonitor(AAdapter:TNetworkAdapter):Boolean; virtual;
   function UnbindMonitor(AAdapter:TNetworkAdapter):Boolean; virtual;
 end;
 
 TAuthenticatorAdapter = class(TListObject) {Downstream}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   Handle:THandle;                //To Do //Do these need lock protection ?
   Adapter:TNetworkAdapter;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;
 
 TNetworkAuthenticator = class(TListObject) {eg EAP/RSN}
   constructor Create(AManager:TTransportManager);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  protected
   {Internal Variables}
   FManager:TTransportManager;
 
   {Status Variables}
   FAuthType:Word;
   FInitDelay:LongWord;
   FRetryCount:LongWord;
   FRetryTimeout:LongWord;
   FAdapters:TNetworkList;   {List of TAuthenticatorAdapter objects}

   {Event Methods}

   {Internal Methods}
   function GetAdapterByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TAuthenticatorAdapter;
   function GetAdapterByAdapter(AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TAuthenticatorAdapter;
   function GetAdapterByNext(APrevious:TAuthenticatorAdapter;ALock,AUnlock:Boolean;AState:LongWord):TAuthenticatorAdapter;

   {Protected Methods}
   function AddAdapter(AAdapter:TNetworkAdapter;AAuthType:Word;ACipher,AKey,AEntity,AToken:Pointer):Boolean; virtual;
   function RemoveAdapter(AAdapter:TNetworkAdapter):Boolean; virtual;
  public
   {Public Properties}
   property Manager:TTransportManager read FManager;
   property AuthType:Word read FAuthType;
   property InitDelay:LongWord read FInitDelay;
   property RetryCount:LongWord read FRetryCount;
   property RetryTimeout:LongWord read FRetryTimeout;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   
   function StartAuthenticator:Boolean; virtual;
   function StopAuthenticator:Boolean; virtual;
   function ProcessAuthenticator:Boolean; virtual;

   function BindAuthenticator(AAdapter:TNetworkAdapter):Boolean; virtual;
   function UnbindAuthenticator(AAdapter:TNetworkAdapter):Boolean; virtual;
 end;
 
 TSocketList = class;
 TSocketState = class;
 TSocketBuffer = class;
 TSocketOptions = class;
 TTransportState = class;
 TTransportOptions = class;
 TTransportSocket = class(TListObject)
   constructor Create(ATransport:TNetworkTransport);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
   FLocalLock:TMutexHandle;
   
   {Internal Methods}
   procedure SetFamily(AFamily:Word);
   procedure SetStruct(AStruct:Word);
   procedure SetProto(AProto:Word); 
   procedure SetOwner(AOwner:TSocketList);
   
   procedure SetSocketError(ASocketError:Integer);
   procedure SetOpenTime(const AOpenTime:Int64);  
   procedure SetCloseTime(const ACloseTime:Int64);  
   procedure SetLingerTime(const ALingerTime:Int64); 
   procedure SetTimewaitTime(const ATimewaitTime:Int64); 
   procedure SetKeepAliveTime(const AKeepAliveTime:Int64);
  protected
   {Internal Variables}
   FTransport:TNetworkTransport;
   
   {Status Variables}
   FFamily:Word;       {AF_INET, AF_INET6, AF_IPX etc}
   FStruct:Word;       {SOCK_STREAM, SOCK_DGRAM etc}
   FProto:Word;        {IPPROTO_UDP, IPPROTO_TCP etc}
   FOwner:TSocketList; {List that this Socket belongs to (eg AcceptQueue)}

   {Socket Layer Variables}
   FSocketError:Integer;          {CONN_REFUSED etc}
   FSocketState:TSocketState;     {SS_UNCONNECTED, SS_ISCONNECTED etc}
   FSocketOptions:TSocketOptions; {SO_ACCEPTCONN, SO_BROADCAST etc}
   FOpenTime:Int64;               {Socket was Opened ar [msec] (TCP_STATE_ESTAB)}
   FCloseTime:Int64;              {Socket was Closed at [msec] (TCP_STATE_CLOSED)}
   FLingerTime:Int64;             {Went into Linger at [msec]}
   FTimewaitTime:Int64;           {Went into Timewait at [msec] (TCP_STATE_TIMEWAIT)}
   FKeepAliveTime:Int64;          {Last KeepAlive Packet at [msec]}

   {Transport Layer Variables}
   FTransportState:TTransportState;
   FTransportOptions:TTransportOptions;

   {Event Methods}

   {Internal Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
  public
   {Status Properties}
   property Family:Word read FFamily write SetFamily;
   property Struct:Word read FStruct write SetStruct;
   property Proto:Word read FProto write SetProto;
   property Owner:TSocketList read FOwner write SetOwner;

   {Socket Layer Properties}
   property SocketError:Integer read FSocketError write SetSocketError;
   property SocketState:TSocketState read FSocketState;
   property SocketOptions:TSocketOptions read FSocketOptions;
   property OpenTime:Int64 read FOpenTime write SetOpenTime;
   property CloseTime:Int64 read FCloseTime write SetCloseTime;
   property LingerTime:Int64 read FLingerTime write SetLingerTime;
   property TimewaitTime:Int64 read FTimewaitTime write SetTimewaitTime;
   property KeepAliveTime:Int64 read FKeepAliveTime write SetKeepAliveTime;

   {Transport Layer Properties}
   property TransportState:TTransportState read FTransportState;
   property TransportOptions:TTransportOptions read FTransportOptions;

   {Public Properties}
   property Transport:TNetworkTransport read FTransport;

   function GetOption(ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer; virtual;
   function SetOption(ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer; virtual;
   function IoCtl(ACommand:DWORD;var AArgument:u_long):Integer; virtual;

   function IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; virtual;
   function IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean; virtual;
   
   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function ReaderConvert:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   function WriterConvert:Boolean;
 end;

 TSocketList = class(TObject) {For tracking Sockets belonging to a List (eg AcceptQueue)} 
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TCriticalSectionHandle;
  protected
   {Internal Variables}
   FList:TList; //To Do //Should this be something better than a TList ?

   {Internal Methods}
   procedure RemoveListItems;
   function GetCount:Integer;
   function GetItem(AIndex:Integer):TTransportSocket;
   procedure SetItem(AIndex:Integer;AItem:TTransportSocket);
  public
   {Public Properties}
   property Count:Integer read GetCount;
   property Items[Index:Integer]:TTransportSocket read GetItem write SetItem;

   {Public Methods}
   function Add(ASocket:TTransportSocket):Integer;
   function Remove(ASocket:TTransportSocket):Integer;
   function First:TTransportSocket;
   function Last:TTransportSocket;
   procedure ClearList;
   
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
 end;

 TSocketState = class(TObject)  {For Socket State tracking SS_UNCONNECTED, SS_PRIV etc}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TCriticalSectionHandle;
  protected
   {Internal Variables}
   FState:LongWord;

   {Internal Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   function GetUnconnected:Boolean;
   procedure SetUnconnected(AUnconnected:Boolean);
   function GetConnected:Boolean;
   procedure SetConnected(AConnected:Boolean);
   function GetConnecting:Boolean;
   procedure SetConnecting(AConnecting:Boolean);
   function GetDisconnecting:Boolean;
   procedure SetDisconnecting(ADisconnecting:Boolean);
   function GetCantSendMore:Boolean;
   procedure SetCantSendMore(ACantSendMore:Boolean);
   function GetCantRecvMore:Boolean;
   procedure SetCantRecvMore(ACantRecvMore:Boolean);
   function GetRecvAtMark:Boolean;
   procedure SetRecvAtMark(ARecvAtMark:Boolean);

   function GetPrivileged:Boolean;
   procedure SetPrivileged(APrivileged:Boolean);
   function GetNonBlocking:Boolean;
   procedure SetNonBlocking(ANonBlocking:Boolean);
   function GetAsync:Boolean;
   procedure SetAsync(AAsync:Boolean);
   function GetListening:Boolean;
   procedure SetListening(AListening:Boolean);

   function GetLocalAddress:Boolean;
   procedure SetLocalAddress(ALocalAddress:Boolean);
   function GetRemoteAddress:Boolean;
   procedure SetRemoteAddress(ARemoteAddress:Boolean);
   function GetConnRefused:Boolean;
   procedure SetConnRefused(AConnRefused:Boolean);
   function GetClosed:Boolean;
   procedure SetClosed(AClosed:Boolean);
  public
   {Public Properties}
   property Unconnected:Boolean read GetUnconnected write SetUnconnected;
   property Connected:Boolean read GetConnected write SetConnected;
   property Connecting:Boolean read GetConnecting write SetConnecting;
   property Disconnecting:Boolean read GetDisconnecting write SetDisconnecting;
   property CantSendMore:Boolean read GetCantSendMore write SetCantSendMore;
   property CantRecvMore:Boolean read GetCantRecvMore write SetCantRecvMore;
   property RecvAtMark:Boolean read GetRecvAtMark write SetRecvAtMark;

   property Privileged:Boolean read GetPrivileged write SetPrivileged;
   property NonBlocking:Boolean read GetNonBlocking write SetNonBlocking;
   property Async:Boolean read GetAsync write SetAsync;
   property Listening:Boolean read GetListening write SetListening;

   property LocalAddress:Boolean read GetLocalAddress write SetLocalAddress;
   property RemoteAddress:Boolean read GetRemoteAddress write SetRemoteAddress;
   property ConnRefused:Boolean read GetConnRefused write SetConnRefused;
   property Closed:Boolean read GetClosed write SetClosed;
 end;

 TSocketBuffer = class(TObject) {Base Socket Buffer Class} 
   constructor Create(ASocket:TTransportSocket);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TMutexHandle; //TCriticalSectionHandle; //TestingRPi
  protected
   {Internal Variables}
   FSocket:TTransportSocket; {Socket owning this buffer}
   FBuffer:TMemoryStream;    {Memory Buffer for Data} //To Do //Should this be a TMemoryStreamEx ?

   FSize:LongWord;           {Physical Size of Data Buffer}
   FStart:Pointer;           {First Byte of Data Buffer}
   FEnd:Pointer;             {Last Byte of Data Buffer}

   {Status Variables}
   FUsed:LongWord;           {Amount of Buffer Used}
   FFree:LongWord;           {Amount of Buffer Free}

   {Event Methods}

   {Protected Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;

   procedure SetSize(ASize:LongWord); virtual;
  public
   {Public Properties}
   property Size:LongWord read FSize write SetSize;

   {Public Methods}
   function GetUsed:LongWord;
   function GetFree:LongWord;
 end;

 TSocketOptions = class(TObject) {For Get/Set Options Level = SOL_SOCKET Option = SO_KEEPALIVE, SO_ACCEPTCONN, SO_BROADCAST etc}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TCriticalSectionHandle;
  protected
   {Internal Variables}
   FOptions:LongWord;

   {Status Variables}
   FLinger:TLinger;
   FSendBuffer:Integer;
   FRecvBuffer:Integer;
   FSendLowMark:Integer;
   FRecvLowMark:Integer;
   FSendTimeout:LongWord;
   FRecvTimeout:LongWord;
   FConnTimeout:LongWord;
   
   {Internal Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
   
   procedure SetLinger(const ALinger:TLinger);
   procedure SetSendBuffer(ASendBuffer:Integer);
   procedure SetRecvBuffer(ARecvBuffer:Integer);
   procedure SetSendLowMark(ASendLowMark:Integer);
   procedure SetRecvLowMark(ARecvLowMark:Integer);
   procedure SetSendTimeout(ASendTimeout:LongWord);
   procedure SetRecvTimeout(ARecvTimeout:LongWord);
   procedure SetConnTimeout(AConnTimeout:LongWord);
   
   function GetDebug:Boolean;
   procedure SetDebug(ADebug:Boolean);
   function GetAccept:Boolean;
   procedure SetAccept(AAccept:Boolean);
   function GetReuseAddress:Boolean;
   procedure SetReuseAddress(AReuseAddress:Boolean);
   function GetKeepAlive:Boolean;
   procedure SetKeepAlive(AKeepAlive:Boolean);
   function GetDontRoute:Boolean;
   procedure SetDontRoute(ADontRoute:Boolean);
   function GetBroadcast:Boolean;
   procedure SetBroadcast(ABroadcast:Boolean);
   function GetUseLoopback:Boolean;
   procedure SetUseLoopback(AUseLoopback:Boolean);
   function GetUrgentInline:Boolean;
   procedure SetUrgentInline(AUrgentInline:Boolean);
  public
   {Public Properties}
   property Linger:TLinger read FLinger write SetLinger;
   property SendBuffer:Integer read FSendBuffer write SetSendBuffer;
   property RecvBuffer:Integer read FRecvBuffer write SetRecvBuffer;
   property SendLowMark:Integer read FSendLowMark write SetSendLowMark;
   property RecvLowMark:Integer read FRecvLowMark write SetRecvLowMark;
   property SendTimeout:LongWord read FSendTimeout write SetSendTimeout;
   property RecvTimeout:LongWord read FRecvTimeout write SetRecvTimeout;
   property ConnTimeout:LongWord read FConnTimeout write SetConnTimeout;

   property Debug:Boolean read GetDebug write SetDebug;
   property Accept:Boolean read GetAccept write SetAccept;
   property ReuseAddress:Boolean read GetReuseAddress write SetReuseAddress;
   property KeepAlive:Boolean read GetKeepAlive write SetKeepAlive;
   property DontRoute:Boolean read GetDontRoute write SetDontRoute;
   property Broadcast:Boolean read GetBroadcast write SetBroadcast;
   property UseLoopback:Boolean read GetUseLoopback write SetUseLoopback;
   property UrgentInline:Boolean read GetUrgentInline write SetUrgentInline;
 end;

 TTransportState = class(TObject)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TCriticalSectionHandle;
  protected
   {Internal Variables}
   
   {Internal Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
  public
   {Status Variables}
   
 end;

 TTransportOptions = class(TObject) {For Get/Set Options at the Transport Level (eg IPPROTO_IP)}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TCriticalSectionHandle;
  protected
   {Internal Variables}
   
   {Internal Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
  public
   {Status Variables}
   
 end;

 THostEntry = class(TListObject)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TCriticalSectionHandle;
   
   {Status Variables}
   FAliases:TStringList; //To Do //Should this be something better than a TStringList ?
   
   {Internal Methods}
   function GetName:String;
   procedure SetName(const AName:String);
   procedure SetFamily(AFamily:Integer);
   procedure SetLength(ALength:Integer);
   procedure SetHostType(AHostType:Word);
   procedure SetHostTime(const AHostTime:Int64);
  protected
   {Status Variables}
   FName:String;
   FFamily:Integer;   {Always 2 for AF_INET}
   FLength:Integer;   {Always 4 for AF_INET}
   FHostType:Word;
   FHostTime:Int64;
  public
   {Public Properties}
   property Name:String read GetName write SetName;
   property Family:Integer read FFamily write SetFamily;
   property Length:Integer read FLength write SetLength;
   property HostType:Word read FHostType write SetHostType;
   property HostTime:Int64 read FHostTime write SetHostTime;

   {Public Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
   
   function FindAlias(const Alias:String):Boolean;
   
   function AddAlias(const Alias:String):Boolean;
   function RemoveAlias(const Alias:String):Boolean;
 end;

 TRouteEntry = class(TListObject)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
   FLocalLock:TMutexHandle;
   
   {Internal Methods}
   procedure SetFamily(AFamily:Integer);
   procedure SetLength(ALength:Integer);
   procedure SetMetric(AMetric:Integer);
   procedure SetRouteType(ARouteType:Word);
   procedure SetRouteTime(const ARouteTime:Int64);
  protected
   {Status Variables}
   FFamily:Integer;   {Always 2 for AF_INET}
   FLength:Integer;   {Always 4 for AF_INET}
   FMetric:Integer;
   FRouteType:Word;
   FRouteTime:Int64;
   
   {Internal Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
  public
   {Public Properties}
   property Family:Integer read FFamily write SetFamily;
   property Length:Integer read FLength write SetLength;
   property Metric:Integer read FMetric write SetMetric;
   property RouteType:Word read FRouteType write SetRouteType;
   property RouteTime:Int64 read FRouteTime write SetRouteTime;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function ReaderConvert:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   function WriterConvert:Boolean;
 end;

 TAddressEntry = class(TListObject)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle; 
   FLocalLock:TMutexHandle;
   
   {Internal Methods}
   procedure SetFamily(AFamily:Integer);
   procedure SetLength(ALength:Integer);
   procedure SetAddressType(AAddressType:Word);
   procedure SetAddressTime(const AAddressTime:Int64);
   procedure SetAdapter(AAdapter:TNetworkAdapter);
  protected
   {Status Variables}
   FFamily:Integer;    {Always 2 for AF_INET}
   FLength:Integer;    {Always 4 for AF_INET}
   FAddressType:Word;  {Dynamic/Static/Multicast/Broadcast/Primary/Secondary}
   FAddressTime:Int64; {Flush time for Dynamic entries}
   FAdapter:TNetworkAdapter;
   
   {Internal Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
  public
   {Public Properties}
   property Family:Integer read FFamily write SetFamily;
   property Length:Integer read FLength write SetLength;
   property AddressType:Word read FAddressType write SetAddressType;
   property AddressTime:Int64 read FAddressTime write SetAddressTime;
   property Adapter:TNetworkAdapter read FAdapter write SetAdapter;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function ReaderConvert:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   function WriterConvert:Boolean;
 end;

 TNetworkEntry = class(TListObject)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TCriticalSectionHandle;
   
   {Status Variables}
   FAliases:TStringList; //To Do //Should this be something better than a TStringList ?
   
   {Internal Methods}
   function GetName:String;
   procedure SetName(const AName:String);
   procedure SetFamily(AFamily:Integer);
   procedure SetLength(ALength:Integer);
  protected
   {Status Variables}
   FName:String;
   FFamily:Integer;   {Always 2 for AF_INET}
   FLength:Integer;   {Always 4 for AF_INET}
  public
   {Public Properties}
   property Name:String read GetName write SetName;
   property Family:Integer read FFamily write SetFamily;
   property Length:Integer read FLength write SetLength;  

   {Public Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
   
   function FindAlias(const Alias:String):Boolean;
   
   function AddAlias(const Alias:String):Boolean;
   function RemoveAlias(const Alias:String):Boolean;
 end;

 TServEntry = class(TListObject)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TCriticalSectionHandle;
   
   {Status Variables}
   FAliases:TStringList; //To Do //Should this be something better than a TStringList ?
   
   {Internal Methods}
   function GetName:String;
   procedure SetName(const AName:String);
   procedure SetPort(APort:Word);
   function GetProtocol:String;
   procedure SetProtocol(const AProtocol:String);
  protected
   {Status Variables}
   FName:String;
   FPort:Word;
   FProtocol:String;
  public
   {Public Properties}
   property Name:String read GetName write SetName;
   property Port:Word read FPort write SetPort;
   property Protocol:String read GetProtocol write SetProtocol;
 
   {Public Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
   
   function FindAlias(const Alias:String):Boolean;
   
   function AddAlias(const Alias:String):Boolean;
   function RemoveAlias(const Alias:String):Boolean;
 end;

 TProtoEntry = class(TListObject)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TCriticalSectionHandle;
   
   {Status Variables}
   FAliases:TStringList; //To Do //Should this be something better than a TStringList ?
   
   {Internal Methods}
   function GetName:String;
   procedure SetName(const AName:String);
   procedure SetNumber(ANumber:Word);
  protected
   {Status Variables}
   FName:String;
   FNumber:Word;
  public
   {Public Properties}
   property Name:String read GetName write SetName;
   property Number:Word read FNumber write SetNumber;
 
   {Public Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
   
   function FindAlias(const Alias:String):Boolean;
   
   function AddAlias(const Alias:String):Boolean;
   function RemoveAlias(const Alias:String):Boolean;
 end;
 
{==============================================================================}
var
 {Transport specific variables}
 TransportManager:TTransportManager;

const 
 {IP4 Transport}
 IP_LOOPBACK_ADDRESS:TInAddr   = (S_addr: $7F000001);  {127.0.0.1}
 IP_LOOPBACK_NETWORK:TInAddr   = (S_addr: $7F000000);  {127.0.0.0}
 IP_LOOPBACK_NETMASK:TInAddr   = (S_addr: $FF000000);  {255.0.0.0}
 IP_BROADCAST_ADDRESS:TInAddr  = (S_addr: $FFFFFFFF);  {255.255.255.255}
 IP_BROADCAST_NETWORK:TInAddr  = (S_addr: $FFFFFFFF);  {255.255.255.255}
 IP_BROADCAST_NETMASK:TInAddr  = (S_addr: $FFFFFFFF);  {255.255.255.255}
 IP_DEFAULT_ADDRESS:TInAddr    = (S_addr: $00000000);  {0.0.0.0}
 IP_DEFAULT_NETWORK:TInAddr    = (S_addr: $00000000);  {0.0.0.0}
 IP_DEFAULT_NETMASK:TInAddr    = (S_addr: $00000000);  {0.0.0.0}
 IP_MULTICAST_HOSTS:TInAddr    = (S_addr: $E0000001);  {224.0.0.1} {All Hosts}
 IP_MULTICAST_ROUTERS:TInAddr  = (S_addr: $E0000002);  {224.0.0.2} {All Routers}
 IP_MULTICAST_NETWORK:TInAddr  = (S_addr: $E0000000);  {224.0.0.0}
 IP_MULTICAST_NETMASK:TInAddr  = (S_addr: $F0000000);  {240.0.0.0}
 {Note: The recommended range for general use is 234.0.0.0 to 238.255.255.255}

 IP_CLASSA_NETMASK:TInAddr = (S_addr: $FF000000);  {255.0.0.0}
 IP_CLASSB_NETMASK:TInAddr = (S_addr: $FFFF0000);  {255.255.0.0}
 IP_CLASSC_NETMASK:TInAddr = (S_addr: $FFFFFF00);  {255.255.255.0}
 IP_CLASSD_NETMASK:TInAddr = (S_addr: $F0000000);  {240.0.0.0}
 IP_CLASSE_NETMASK:TInAddr = (S_addr: $F0000000);  {240.0.0.0}

 {IP6 Transport}
 IP6_LOOPBACK_ADDRESS:TIn6Addr = (S6_addr: ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01));
 IP6_DEFAULT_ADDRESS:TIn6Addr = (S6_addr: ($00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00));
 
{==============================================================================}
{Initialization Functions}
procedure TransportInit;
function TransportStart:LongWord;
function TransportStop:LongWord;

function TransportBind:LongWord;
function TransportUnbind:LongWord;

{==============================================================================}
{Transport Functions}
function InAddrToHost(const AAddress:TInAddr):TInAddr; inline;
function InAddrToNetwork(const AAddress:TInAddr):TInAddr; inline;

function InAddrToString(const AAddress:TInAddr):String;
function StringToInAddr(const AAddress:String):TInAddr;

function InAddrIsEqual(const AAddress1,AAddress2:TInAddr):Boolean;
function InAddrIsDefault(const AAddress:TInAddr):Boolean;
function InAddrIsLoopback(const AAddress:TInAddr):Boolean;
function InAddrIsBroadcast(const AAddress:TInAddr):Boolean;
function InAddrIsMulticast(const AAddress:TInAddr):Boolean;

function In6AddrToString(const AAddress:TIn6Addr):String;
function StringToIn6Addr(const AAddress:String):TIn6Addr;

function In6AddrIsEqual(const AAddress1,AAddress2:TIn6Addr):Boolean;
function In6AddrIsDefault(const AAddress:TIn6Addr):Boolean;
function In6AddrIsLoopback(const AAddress:TIn6Addr):Boolean;
function In6AddrIsLinkLocal(const AAddress:TIn6Addr):Boolean;
function In6AddrIsSiteLocal(const AAddress:TIn6Addr):Boolean;
function In6AddrIsV4Mapped(const AAddress:TIn6Addr):Boolean;
function In6AddrIsV4Compatible(const AAddress:TIn6Addr):Boolean;
function In6AddrIsMulticast(const AAddress:TIn6Addr):Boolean;
function In6AddrIsMulticastNodeLocal(const AAddress:TIn6Addr):Boolean;
function In6AddrIsMulticastLinkLocal(const AAddress:TIn6Addr):Boolean;
function In6AddrIsMulticastSiteLocal(const AAddress:TIn6Addr):Boolean;
function In6AddrIsMulticastOrgLocal(const AAddress:TIn6Addr):Boolean;
function In6AddrIsMulticastGlobal(const AAddress:TIn6Addr):Boolean;

function IpxAddrToString(const AAddress:TIpxAddr):String;
function StringToIpxAddr(const AAddress:String):TIpxAddr;

function GetChecksum(ABuffer:Pointer;AOffset,ALength:Word):Word;
function GetChecksum2(APseudo,ABuffer:Pointer;APseudoLength,ABufferOffset,ABufferLength:Word):Word;
function GetChecksum3(APseudo,AHeader,AData:Pointer;APseudoLength,AHeaderLength,ADataOffset,ADataLength:Word):Word;
function GetChecksum4(APseudo,AHeader,AOptions,AData:Pointer;APseudoLength,AHeaderLength,AOptionsLength,ADataOffset,ADataLength:Word):Word;
 
{==============================================================================}
{Transport Helper Functions}
function HostTypeToString(AType:Word):String;
function RouteTypeToString(AType:Word):String;
function AddressTypeToString(AType:Word):String;

function ProtocolToString(AProtocol:Word):String;
function SocketTypeToString(ASocketType:Word):String;
function AddressFamilyToString(AFamily:Word):String;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Transport specific variables}
 TransportInitialized:Boolean;
 TransportStarted:Boolean;

{==============================================================================}
{==============================================================================}
{TTransportManager}
constructor TTransportManager.Create(ASettings:TNetworkSettings;AAdapters:TAdapterManager);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 FSettings:=ASettings;
 FAdapters:=AAdapters;
 
 FTransports:=TNetworkList.Create;
 FMonitors:=TNetworkList.Create;
 FAuthenticators:=TNetworkList.Create;
end;

{==============================================================================}

destructor TTransportManager.Destroy;
begin
 {}
 WriterLock;
 try
  FAuthenticators.Free;
  FMonitors.Free;
  FTransports.Free;
  FSettings:=nil;
  FAdapters:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TTransportManager.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportManager.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportManager.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportManager.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportManager.AddTransport(ATransport:TNetworkTransport):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: AddTransport');
  {$ENDIF}

  {Acquire Lock}
  FTransports.WriterLock;
  try
   {Add Transport}
   Result:=FTransports.Add(ATransport);
  finally
   {Release Lock}
   FTransports.WriterUnlock;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.RemoveTransport(ATransport:TNetworkTransport):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: RemoveTransport');
  {$ENDIF}

  {Acquire Lock}
  FTransports.WriterLock;
  try
   {Remove Transport}
   Result:=FTransports.Remove(ATransport);
  finally
   {Release Lock}  
   FTransports.WriterUnlock;
  end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.GetTransportByType(AFamily,APacketType:Word;ALock:Boolean;AState:LongWord):TNetworkTransport;
var
 Transport:TNetworkTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Get Transport}
  Transport:=TNetworkTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if (Transport.Family = AFamily) and (Transport.PacketType = APacketType) then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;
      
      {Return Result}
      Result:=Transport;
      Exit;
     end;
     
    {Get Next} 
    Transport:=TNetworkTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.GetTransportByTransport(ATransport:TNetworkTransport;ALock:Boolean;AState:LongWord):TNetworkTransport;
var
 Transport:TNetworkTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Get Transport}
  Transport:=TNetworkTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if Transport = ATransport then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;
      
      {Return Result}
      Result:=Transport;
      Exit;
     end;
     
    {Get Next} 
    Transport:=TNetworkTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.GetTransportByNext(APrevious:TNetworkTransport;ALock,AUnlock:Boolean;AState:LongWord):TNetworkTransport;
var
 Transport:TNetworkTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Transport:=TNetworkTransport(FTransports.First);
    if Transport <> nil then
     begin
      {Lock Transport}
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;
      
      {Return Result}
      Result:=Transport;
     end;
   end
  else
   begin
    {Get Next}
    Transport:=TNetworkTransport(APrevious.Next);
    if Transport <> nil then
     begin
      {Lock Transport}
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;
      
      {Return Result}
      Result:=Transport;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FTransports.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.AddMonitor(AMonitor:TNetworkMonitor):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: AddMonitor');
  {$ENDIF}

  {Acquire Lock}
  FMonitors.WriterLock;
  try
   {Add Monitor}
   Result:=FMonitors.Add(AMonitor);
  finally
   {Release Lock}
   FMonitors.WriterUnlock;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.RemoveMonitor(AMonitor:TNetworkMonitor):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: RemoveMonitor');
  {$ENDIF}

  {Acquire Lock}
  FMonitors.WriterLock;
  try
   {Remove Monitor}
   Result:=FMonitors.Remove(AMonitor);
  finally
   {Release Lock}  
   FMonitors.WriterUnlock;
  end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.GetMonitorByMonitor(AMonitor:TNetworkMonitor;ALock:Boolean;AState:LongWord):TNetworkMonitor;
var
 Monitor:TNetworkMonitor;
begin
 {}
 FMonitors.ReaderLock;
 try
  Result:=nil;
  
  {Get Monitor}
  Monitor:=TNetworkMonitor(FMonitors.First);
  while Monitor <> nil do
   begin
    {Check Monitor}
    if Monitor = AMonitor then
     begin
      {Lock Monitor} 
      if ALock then if AState = NETWORK_LOCK_READ then Monitor.ReaderLock else Monitor.WriterLock;
      
      {Return Result}
      Result:=Monitor;
      Exit;
     end;
     
    {Get Next} 
    Monitor:=TNetworkMonitor(Monitor.Next);
   end;
 finally 
  FMonitors.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.GetMonitorByNext(APrevious:TNetworkMonitor;ALock,AUnlock:Boolean;AState:LongWord):TNetworkMonitor;
var
 Monitor:TNetworkMonitor;
begin
 {}
 FMonitors.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Monitor:=TNetworkMonitor(FMonitors.First);
    if Monitor <> nil then
     begin
      {Lock Monitor}
      if ALock then if AState = NETWORK_LOCK_READ then Monitor.ReaderLock else Monitor.WriterLock;
      
      {Return Result}
      Result:=Monitor;
     end;
   end
  else
   begin
    {Get Next}
    Monitor:=TNetworkMonitor(APrevious.Next);
    if Monitor <> nil then
     begin
      {Lock Monitor}
      if ALock then if AState = NETWORK_LOCK_READ then Monitor.ReaderLock else Monitor.WriterLock;
      
      {Return Result}
      Result:=Monitor;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FMonitors.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.AddAuthenticator(AAuthenticator:TNetworkAuthenticator):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: AddAuthenticator');
  {$ENDIF}

  {Acquire Lock}
  FAuthenticators.WriterLock;
  try
   {Add Authenticator}
   Result:=FAuthenticators.Add(AAuthenticator);
  finally
   {Release Lock}
   FAuthenticators.WriterUnlock;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.RemoveAuthenticator(AAuthenticator:TNetworkAuthenticator):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: RemoveAuthenticator');
  {$ENDIF}

  {Acquire Lock}
  FAuthenticators.WriterLock;
  try
   {Remove Authenticator}
   Result:=FAuthenticators.Remove(AAuthenticator);
  finally
   {Release Lock}  
   FAuthenticators.WriterUnlock;
  end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.GetAuthenticatorByType(AAuthType:Word;ALock:Boolean;AState:LongWord):TNetworkAuthenticator;
var
 Authenticator:TNetworkAuthenticator;
begin
 {}
 FAuthenticators.ReaderLock;
 try
  Result:=nil;
  
  {Get Authenticator}
  Authenticator:=TNetworkAuthenticator(FAuthenticators.First);
  while Authenticator <> nil do
   begin
    {Check Authenticator}
    if Authenticator.AuthType = AAuthType then
     begin
      {Lock Authenticator} 
      if ALock then if AState = NETWORK_LOCK_READ then Authenticator.ReaderLock else Authenticator.WriterLock;
      
      {Return Result}
      Result:=Authenticator;
      Exit;
     end;
     
    {Get Next} 
    Authenticator:=TNetworkAuthenticator(Authenticator.Next);
   end;
 finally 
  FAuthenticators.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.GetAuthenticatorByAuthenticator(AAuthenticator:TNetworkAuthenticator;ALock:Boolean;AState:LongWord):TNetworkAuthenticator;
var
 Authenticator:TNetworkAuthenticator;
begin
 {}
 FAuthenticators.ReaderLock;
 try
  Result:=nil;
  
  {Get Authenticator}
  Authenticator:=TNetworkAuthenticator(FAuthenticators.First);
  while Authenticator <> nil do
   begin
    {Check Authenticator}
    if Authenticator = AAuthenticator then
     begin
      {Lock Authenticator} 
      if ALock then if AState = NETWORK_LOCK_READ then Authenticator.ReaderLock else Authenticator.WriterLock;
      
      {Return Result}
      Result:=Authenticator;
      Exit;
     end;
     
    {Get Next} 
    Authenticator:=TNetworkAuthenticator(Authenticator.Next);
   end;
 finally 
  FAuthenticators.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.GetAuthenticatorByNext(APrevious:TNetworkAuthenticator;ALock,AUnlock:Boolean;AState:LongWord):TNetworkAuthenticator;
var
 Authenticator:TNetworkAuthenticator;
begin
 {}
 FAuthenticators.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Authenticator:=TNetworkAuthenticator(FAuthenticators.First);
    if Authenticator <> nil then
     begin
      {Lock Authenticator}
      if ALock then if AState = NETWORK_LOCK_READ then Authenticator.ReaderLock else Authenticator.WriterLock;
      
      {Return Result}
      Result:=Authenticator;
     end;
   end
  else
   begin
    {Get Next}
    Authenticator:=TNetworkAuthenticator(APrevious.Next);
    if Authenticator <> nil then
     begin
      {Lock Authenticator}
      if ALock then if AState = NETWORK_LOCK_READ then Authenticator.ReaderLock else Authenticator.WriterLock;
      
      {Return Result}
      Result:=Authenticator;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FAuthenticators.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.StartTransports:Boolean;
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: StartTransports');
  {$ENDIF}
  
  {Get Transport}
  Transport:=TNetworkTransport(GetTransportByNext(nil,True,False,NETWORK_LOCK_READ));
  while Transport <> nil do
   begin
    {Start Transport}   
    if not(Transport.StartTransport) then Result:=False;
    
    {Get Next}
    Transport:=TNetworkTransport(GetTransportByNext(Transport,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.StopTransports:Boolean;
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: StopTransports');
  {$ENDIF}
  
  {Get Transport}
  Transport:=TNetworkTransport(GetTransportByNext(nil,True,False,NETWORK_LOCK_READ));
  while Transport <> nil do
   begin
    {Stop Transport}
    if not(Transport.StopTransport) then Result:=False;
    
    {Get Next}
    Transport:=TNetworkTransport(GetTransportByNext(Transport,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.ProcessTransports:Boolean;
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: ProcessTransports');
  {$ENDIF}
  
  {Get Transport}
  Transport:=TNetworkTransport(GetTransportByNext(nil,True,False,NETWORK_LOCK_READ));
  while Transport <> nil do
   begin
    {Process Transport}
    if not(Transport.ProcessTransport) then Result:=False;
    
    {Get Next}
    Transport:=TNetworkTransport(GetTransportByNext(Transport,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.EnumerateTransports(ACallback:TTransportCallback):Boolean;
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {Check Callback}
  if not Assigned(ACallback) then Exit;

  Result:=True;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: EnumerateTransports');
  {$ENDIF}
  
  {Get Transport}
  Transport:=TNetworkTransport(GetTransportByNext(nil,True,False,NETWORK_LOCK_READ));
  while Transport <> nil do
   begin
    {Enumerate Transport}
    if not(ACallback(Transport)) then Result:=False;
    
    {Get Next}
    Transport:=TNetworkTransport(GetTransportByNext(Transport,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.BindTransports(AAdapter:TNetworkAdapter):Boolean;
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: BindTransports');
  {$ENDIF}
  
  {Check Adapter}
  if AAdapter = nil then
   begin
    {Check Adapters}
    if FAdapters = nil then Exit;
    
    {Enumerate Adapters}
    Result:=FAdapters.EnumerateAdapters(BindTransports);
   end
  else
   begin
    Result:=True;
   
    {Get Transport}
    Transport:=TNetworkTransport(GetTransportByNext(nil,True,False,NETWORK_LOCK_READ));
    while Transport <> nil do
     begin
      {Bind Transport}
      if not(Transport.BindTransport(AAdapter)) then Result:=False;
    
      {Get Next}
      Transport:=TNetworkTransport(GetTransportByNext(Transport,True,True,NETWORK_LOCK_READ));
     end;
   end;   
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.UnbindTransports(AAdapter:TNetworkAdapter):Boolean;
var
 Transport:TNetworkTransport;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: UnbindTransports');
  {$ENDIF}
  
  {Check Adapter}
  if AAdapter = nil then
   begin
    {Check Adapters}
    if FAdapters = nil then Exit;
    
    {Enumerate Adapters}
    Result:=FAdapters.EnumerateAdapters(UnbindTransports);
   end
  else
   begin
    Result:=True;
   
    {Get Transport}
    Transport:=TNetworkTransport(GetTransportByNext(nil,True,False,NETWORK_LOCK_READ));
    while Transport <> nil do
     begin
      {Unbind Transport}
      if not(Transport.UnbindTransport(AAdapter)) then Result:=False;
    
      {Get Next}
      Transport:=TNetworkTransport(GetTransportByNext(Transport,True,True,NETWORK_LOCK_READ));
     end;
   end;   
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.StartMonitors:Boolean;
var
 Monitor:TNetworkMonitor;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: StartMonitors');
  {$ENDIF}
  
  {Get Monitor}
  Monitor:=TNetworkMonitor(GetMonitorByNext(nil,True,False,NETWORK_LOCK_READ));
  while Monitor <> nil do
   begin
    {Start Monitor}   
    if not(Monitor.StartMonitor) then Result:=False;
    
    {Get Next}
    Monitor:=TNetworkMonitor(GetMonitorByNext(Monitor,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.StopMonitors:Boolean;
var
 Monitor:TNetworkMonitor;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: StopMonitors');
  {$ENDIF}
  
  {Get Monitor}
  Monitor:=TNetworkMonitor(GetMonitorByNext(nil,True,False,NETWORK_LOCK_READ));
  while Monitor <> nil do
   begin
    {Stop Monitor}
    if not(Monitor.StopMonitor) then Result:=False;
    
    {Get Next}
    Monitor:=TNetworkMonitor(GetMonitorByNext(Monitor,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.ProcessMonitors:Boolean;
var
 Monitor:TNetworkMonitor;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: ProcessMonitors');
  {$ENDIF}
  
  {Get Monitor}
  Monitor:=TNetworkMonitor(GetMonitorByNext(nil,True,False,NETWORK_LOCK_READ));
  while Monitor <> nil do
   begin
    {Process Monitor}
    if not(Monitor.ProcessMonitor) then Result:=False;
    
    {Get Next}
    Monitor:=TNetworkMonitor(GetMonitorByNext(Monitor,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.EnumerateMonitors(ACallback:TMonitorCallback):Boolean;
var
 Monitor:TNetworkMonitor;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {Check Callback}
  if not Assigned(ACallback) then Exit;

  Result:=True;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: EnumerateMonitors');
  {$ENDIF}
  
  {Get Monitor}
  Monitor:=TNetworkMonitor(GetMonitorByNext(nil,True,False,NETWORK_LOCK_READ));
  while Monitor <> nil do
   begin
    {Enumerate Monitor}
    if not(ACallback(Monitor)) then Result:=False;
    
    {Get Next}
    Monitor:=TNetworkMonitor(GetMonitorByNext(Monitor,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.BindMonitors(AAdapter:TNetworkAdapter):Boolean;
var
 Monitor:TNetworkMonitor;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: BindMonitors');
  {$ENDIF}
  
  {Check Adapter}
  if AAdapter = nil then
   begin
    {Check Adapters}
    if FAdapters = nil then Exit;
    
    {Enumerate Adapters}
    Result:=FAdapters.EnumerateAdapters(BindMonitors);
   end
  else
   begin
    Result:=True;
   
    {Get Monitor}
    Monitor:=TNetworkMonitor(GetMonitorByNext(nil,True,False,NETWORK_LOCK_READ));
    while Monitor <> nil do
     begin
      {Bind Monitor}
      if not(Monitor.BindMonitor(AAdapter)) then Result:=False;
    
      {Get Next}
      Monitor:=TNetworkMonitor(GetMonitorByNext(Monitor,True,True,NETWORK_LOCK_READ));
     end;
   end;   
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.UnbindMonitors(AAdapter:TNetworkAdapter):Boolean;
var
 Monitor:TNetworkMonitor;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: UnbindMonitors');
  {$ENDIF}
  
  {Check Adapter}
  if AAdapter = nil then
   begin
    {Check Adapters}
    if FAdapters = nil then Exit;
    
    {Enumerate Adapters}
    Result:=FAdapters.EnumerateAdapters(UnbindMonitors);
   end
  else
   begin
    Result:=True;
   
    {Get Monitor}
    Monitor:=TNetworkMonitor(GetMonitorByNext(nil,True,False,NETWORK_LOCK_READ));
    while Monitor <> nil do
     begin
      {Unbind Monitor}
      if not(Monitor.UnbindMonitor(AAdapter)) then Result:=False;
    
      {Get Next}
      Monitor:=TNetworkMonitor(GetMonitorByNext(Monitor,True,True,NETWORK_LOCK_READ));
     end;
   end;   
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.StartAuthenticators:Boolean;
var
 Authenticator:TNetworkAuthenticator;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: StartAuthenticators');
  {$ENDIF}
  
  {Get Authenticator}
  Authenticator:=TNetworkAuthenticator(GetAuthenticatorByNext(nil,True,False,NETWORK_LOCK_READ));
  while Authenticator <> nil do
   begin
    {Start Authenticator}   
    if not(Authenticator.StartAuthenticator) then Result:=False;
    
    {Get Next}
    Authenticator:=TNetworkAuthenticator(GetAuthenticatorByNext(Authenticator,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.StopAuthenticators:Boolean;
var
 Authenticator:TNetworkAuthenticator;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: StopAuthenticators');
  {$ENDIF}
  
  {Get Authenticator}
  Authenticator:=TNetworkAuthenticator(GetAuthenticatorByNext(nil,True,False,NETWORK_LOCK_READ));
  while Authenticator <> nil do
   begin
    {Stop Authenticator}
    if not(Authenticator.StopAuthenticator) then Result:=False;
    
    {Get Next}
    Authenticator:=TNetworkAuthenticator(GetAuthenticatorByNext(Authenticator,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.ProcessAuthenticators:Boolean;
var
 Authenticator:TNetworkAuthenticator;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: ProcessAuthenticators');
  {$ENDIF}
  
  {Get Authenticator}
  Authenticator:=TNetworkAuthenticator(GetAuthenticatorByNext(nil,True,False,NETWORK_LOCK_READ));
  while Authenticator <> nil do
   begin
    {Process Authenticator}
    if not(Authenticator.ProcessAuthenticator) then Result:=False;
    
    {Get Next}
    Authenticator:=TNetworkAuthenticator(GetAuthenticatorByNext(Authenticator,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.EnumerateAuthenticators(ACallback:TAuthenticatorCallback):Boolean;
var
 Authenticator:TNetworkAuthenticator;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {Check Callback}
  if not Assigned(ACallback) then Exit;

  Result:=True;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: EnumerateAuthenticators');
  {$ENDIF}
  
  {Get Authenticator}
  Authenticator:=TNetworkAuthenticator(GetAuthenticatorByNext(nil,True,False,NETWORK_LOCK_READ));
  while Authenticator <> nil do
   begin
    {Enumerate Authenticator}
    if not(ACallback(Authenticator)) then Result:=False;
    
    {Get Next}
    Authenticator:=TNetworkAuthenticator(GetAuthenticatorByNext(Authenticator,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.BindAuthenticators(AAdapter:TNetworkAdapter):Boolean;
var
 Authenticator:TNetworkAuthenticator;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: BindAuthenticators');
  {$ENDIF}
  
  {Check Adapter}
  if AAdapter = nil then
   begin
    {Check Adapters}
    if FAdapters = nil then Exit;
    
    {Enumerate Adapters}
    Result:=FAdapters.EnumerateAdapters(BindAuthenticators);
   end
  else
   begin
    Result:=True;
   
    {Get Authenticator}
    Authenticator:=TNetworkAuthenticator(GetAuthenticatorByNext(nil,True,False,NETWORK_LOCK_READ));
    while Authenticator <> nil do
     begin
      {Bind Authenticator}
      if not(Authenticator.BindAuthenticator(AAdapter)) then Result:=False;
    
      {Get Next}
      Authenticator:=TNetworkAuthenticator(GetAuthenticatorByNext(Authenticator,True,True,NETWORK_LOCK_READ));
     end;
   end;   
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportManager.UnbindAuthenticators(AAdapter:TNetworkAdapter):Boolean;
var
 Authenticator:TNetworkAuthenticator;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF TRANSPORT_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'TransportManager: UnbindAuthenticators');
  {$ENDIF}
  
  {Check Adapter}
  if AAdapter = nil then
   begin
    {Check Adapters}
    if FAdapters = nil then Exit;
    
    {Enumerate Adapters}
    Result:=FAdapters.EnumerateAdapters(UnbindAuthenticators);
   end
  else
   begin
    Result:=True;
   
    {Get Authenticator}
    Authenticator:=TNetworkAuthenticator(GetAuthenticatorByNext(nil,True,False,NETWORK_LOCK_READ));
    while Authenticator <> nil do
     begin
      {Unbind Authenticator}
      if not(Authenticator.UnbindAuthenticator(AAdapter)) then Result:=False;
    
      {Get Next}
      Authenticator:=TNetworkAuthenticator(GetAuthenticatorByNext(Authenticator,True,True,NETWORK_LOCK_READ));
     end;
   end;   
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TTransportBuffer}
constructor TTransportBuffer.Create(ATransport:TNetworkTransport);
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FTransport:=ATransport;
end;

{==============================================================================}

destructor TTransportBuffer.Destroy;
begin
 {}
 AcquireLock;
 try
  FTransport:=nil;
  inherited Destroy;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TTransportBuffer.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportBuffer.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TTransportBufferEx}
constructor TTransportBufferEx.Create(ATransport:TNetworkTransport);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 FTransport:=ATransport;
end;

{==============================================================================}

destructor TTransportBufferEx.Destroy;
begin
 {}
 WriterLock;
 try
  FTransport:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TTransportBufferEx.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportBufferEx.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportBufferEx.ReaderConvert:Boolean; 
{Convert a Reader lock to a Writer lock}
begin
 {}
 Result:=(SynchronizerReaderConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportBufferEx.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportBufferEx.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportBufferEx.WriterConvert:Boolean;
{Convert a Writer lock to a Reader lock}
begin
 {}
 Result:=(SynchronizerWriterConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportBufferEx.WriterOwner:Boolean;
{Return True if the current thread is the writer owner}
begin
 {}
 Result:=(SynchronizerWriterOwner(FLock) = GetCurrentThreadID);
end;

{==============================================================================}
{==============================================================================}
{TTransportAdapter}
constructor TTransportAdapter.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 Name:='';
 Index:=LongWord(Self);
 Handle:=INVALID_HANDLE_VALUE;
 PacketType:=PACKET_TYPE_RAW;
 Adapter:=nil;
 Hardware:=HARDWARE_DEFAULT;
 Broadcast:=HARDWARE_BROADCAST;

 MTU:=0;

 ConfigType:=CONFIG_TYPE_AUTO;
 Configured:=False;
 Configuring:=False;
end;

{==============================================================================}

destructor TTransportAdapter.Destroy;
begin
 {}
 WriterLock;
 try
  Adapter:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TTransportAdapter.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportAdapter.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportAdapter.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportAdapter.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TTransportBinding}
constructor TTransportBinding.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 Name:='';
 Index:=LongWord(Self);
 Adapter:=nil;

 ConfigType:=CONFIG_TYPE_AUTO;
 Configured:=False;
 Configuring:=False;
end;

{==============================================================================}

destructor TTransportBinding.Destroy;
begin
 {}
 WriterLock;
 try
  Adapter:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TTransportBinding.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportBinding.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportBinding.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportBinding.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TTransportProtocol}
constructor TTransportProtocol.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 Protocol:=IPPROTO_IP;
 PacketHandler:=nil;
 ControlHandler:=nil;
end;

{==============================================================================}

destructor TTransportProtocol.Destroy;
begin
 {}
 WriterLock;
 try
  ControlHandler:=nil;
  PacketHandler:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TTransportProtocol.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportProtocol.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportProtocol.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportProtocol.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TTransportFilter}
constructor TTransportFilter.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 Protocol:=IPPROTO_IP;
 FilterHandler:=nil;
end;

{==============================================================================}

destructor TTransportFilter.Destroy;
begin
 {}
 WriterLock;
 try
  FilterHandler:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TTransportFilter.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportFilter.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportFilter.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportFilter.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TTransportConfig}
constructor TTransportConfig.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 ConfigType:=CONFIG_TYPE_AUTO;
 ConfigAuto:=True;
 ConfigHandler:=nil;
end;

{==============================================================================}

destructor TTransportConfig.Destroy;
begin
 {}
 WriterLock;
 try
  ConfigHandler:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TTransportConfig.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportConfig.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportConfig.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportConfig.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TNetworkTransport}
constructor TNetworkTransport.Create(AManager:TTransportManager;const AName:String);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 FManager:=AManager;
 FName:=AName;
 
 FFamily:=AF_UNSPEC;
 FPacketType:=PACKET_TYPE_RAW;
 FAdapters:=TNetworkList.Create;
 FBindings:=TNetworkList.Create;
 FProtocols:=TNetworkList.Create;
 FFilters:=TNetworkList.Create;
 FConfigs:=TNetworkList.Create;
 FillChar(FStatistics,SizeOf(TTransportStatistics),0);
 if FManager <> nil then FManager.AddTransport(Self);
end;

{==============================================================================}

destructor TNetworkTransport.Destroy;
begin
 {}
 WriterLock;
 try
  if FManager <> nil then FManager.RemoveTransport(Self);
  FConfigs.Free;
  FFilters.Free;
  FProtocols.Free;
  FBindings.Free;
  FAdapters.Free;
  FPacketType:=PACKET_TYPE_RAW;
  FFamily:=AF_UNSPEC;
  FManager:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TNetworkTransport.GetAdapterByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TTransportAdapter;
var
 Adapter:TTransportAdapter;
begin
 {}
 FAdapters.ReaderLock;
 try
  Result:=nil;
  
  {Get Adapter}
  Adapter:=TTransportAdapter(FAdapters.First);
  while Adapter <> nil do
   begin
    {Check Adapter}
    if Adapter.Handle = AHandle then
     begin
      {Lock Adapter} 
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
      Exit;
     end;
     
    {Get Next} 
    Adapter:=TTransportAdapter(Adapter.Next);
   end;
 finally 
  FAdapters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.GetAdapterByAdapter(AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TTransportAdapter;
var
 Adapter:TTransportAdapter;
begin
 {}
 FAdapters.ReaderLock;
 try
  Result:=nil;
  
  {Get Adapter}
  Adapter:=TTransportAdapter(FAdapters.First);
  while Adapter <> nil do
   begin
    {Check Adapter}
    if Adapter.Adapter = AAdapter then
     begin
      {Lock Adapter} 
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
      Exit;
     end;
     
    {Get Next} 
    Adapter:=TTransportAdapter(Adapter.Next);
   end;
 finally 
  FAdapters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.GetAdapterByNext(APrevious:TTransportAdapter;ALock,AUnlock:Boolean;AState:LongWord):TTransportAdapter;
var
 Adapter:TTransportAdapter;
begin
 {}
 FAdapters.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Adapter:=TTransportAdapter(FAdapters.First);
    if Adapter <> nil then
     begin
      {Lock Adapter}
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
     end;
   end
  else
   begin
    {Get Next}
    Adapter:=TTransportAdapter(APrevious.Next);
    if Adapter <> nil then
     begin
      {Lock Adapter}
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FAdapters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.GetProtocolByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TTransportProtocol;
var
 Protocol:TTransportProtocol;
begin
 {}
 FProtocols.ReaderLock;
 try
  Result:=nil;
  
  {Get Protocol}
  Protocol:=TTransportProtocol(FProtocols.First);
  while Protocol <> nil do
   begin
    {Check Protocol}
    if THandle(Protocol) = AHandle then
     begin
      {Lock Protocol} 
      if ALock then if AState = NETWORK_LOCK_READ then Protocol.ReaderLock else Protocol.WriterLock;
      
      {Return Result}
      Result:=Protocol;
      Exit;
     end;
     
    {Get Next} 
    Protocol:=TTransportProtocol(Protocol.Next);
   end;
 finally 
  FProtocols.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.GetProtocolByType(AProtocol:Word;ALock:Boolean;AState:LongWord):TTransportProtocol;
var
 Protocol:TTransportProtocol;
begin
 {}
 FProtocols.ReaderLock;
 try
  Result:=nil;
  
  {Get Protocol}
  Protocol:=TTransportProtocol(FProtocols.First);
  while Protocol <> nil do
   begin
    {Check Protocol}
    if Protocol.Protocol = AProtocol then
     begin
      {Lock Protocol} 
      if ALock then if AState = NETWORK_LOCK_READ then Protocol.ReaderLock else Protocol.WriterLock;
      
      {Return Result}
      Result:=Protocol;
      Exit;
     end;
     
    {Get Next}
    Protocol:=TTransportProtocol(Protocol.Next);
   end;
 finally 
  FProtocols.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.GetProtocolByProtocol(AProtocol:TTransportProtocol;ALock:Boolean;AState:LongWord):TTransportProtocol;
var
 Protocol:TTransportProtocol;
begin
 {}
 FProtocols.ReaderLock;
 try
  Result:=nil;
  
  {Get Protocol}
  Protocol:=TTransportProtocol(FProtocols.First);
  while Protocol <> nil do
   begin
    {Check Protocol}
    if Protocol = AProtocol then
     begin
      {Lock Protocol} 
      if ALock then if AState = NETWORK_LOCK_READ then Protocol.ReaderLock else Protocol.WriterLock;
      
      {Return Result}
      Result:=Protocol;
      Exit;
     end;
     
    {Get Next} 
    Protocol:=TTransportProtocol(Protocol.Next);
   end;
 finally 
  FProtocols.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.GetProtocolByNext(APrevious:TTransportProtocol;ALock,AUnlock:Boolean;AState:LongWord):TTransportProtocol;
var
 Protocol:TTransportProtocol;
begin
 {}
 FProtocols.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Protocol:=TTransportProtocol(FProtocols.First);
    if Protocol <> nil then
     begin
      {Lock Protocol}
      if ALock then if AState = NETWORK_LOCK_READ then Protocol.ReaderLock else Protocol.WriterLock;
      
      {Return Result}
      Result:=Protocol;
     end;
   end
  else
   begin
    {Get Next}
    Protocol:=TTransportProtocol(APrevious.Next);
    if Protocol <> nil then
     begin
      {Lock Protocol}
      if ALock then if AState = NETWORK_LOCK_READ then Protocol.ReaderLock else Protocol.WriterLock;
      
      {Return Result}
      Result:=Protocol;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FProtocols.ReaderUnlock;
 end; 
end;
  

{==============================================================================}

function TNetworkTransport.GetFilterByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TTransportFilter;
var
 Filter:TTransportFilter;
begin
 {}
 FFilters.ReaderLock;
 try
  Result:=nil;
  
  {Get Filter}
  Filter:=TTransportFilter(FFilters.First);
  while Filter <> nil do
   begin
    {Check Filter}
    if THandle(Filter) = AHandle then
     begin
      {Lock Filter} 
      if ALock then if AState = NETWORK_LOCK_READ then Filter.ReaderLock else Filter.WriterLock;
      
      {Return Result}
      Result:=Filter;
      Exit;
     end;
     
    {Get Next} 
    Filter:=TTransportFilter(Filter.Next);
   end;
 finally 
  FFilters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.GetFilterByProtocol(AProtocol:Word;ALock:Boolean;AState:LongWord):TTransportFilter;
var
 Filter:TTransportFilter;
begin
 {}
 FFilters.ReaderLock;
 try
  Result:=nil;
  
  {Get Filter}
  Filter:=TTransportFilter(FFilters.First);
  while Filter <> nil do
   begin
    {Check Filter}
    if Filter.Protocol = AProtocol then
     begin
      {Lock Filter} 
      if ALock then if AState = NETWORK_LOCK_READ then Filter.ReaderLock else Filter.WriterLock;
      
      {Return Result}
      Result:=Filter;
      Exit;
     end;
     
    {Get Next} 
    Filter:=TTransportFilter(Filter.Next);
   end;
 finally 
  FFilters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.GetFilterByFilter(AFilter:TTransportFilter;ALock:Boolean;AState:LongWord):TTransportFilter;
var
 Filter:TTransportFilter;
begin
 {}
 FFilters.ReaderLock;
 try
  Result:=nil;
  
  {Get Filter}
  Filter:=TTransportFilter(FFilters.First);
  while Filter <> nil do
   begin
    {Check Filter}
    if Filter = AFilter then
     begin
      {Lock Filter} 
      if ALock then if AState = NETWORK_LOCK_READ then Filter.ReaderLock else Filter.WriterLock;
      
      {Return Result}
      Result:=Filter;
      Exit;
     end;
     
    {Get Next} 
    Filter:=TTransportFilter(Filter.Next);
   end;
 finally 
  FFilters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.GetFilterByNext(APrevious:TTransportFilter;ALock,AUnlock:Boolean;AState:LongWord):TTransportFilter;
var
 Filter:TTransportFilter;
begin
 {}
 FFilters.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Filter:=TTransportFilter(FFilters.First);
    if Filter <> nil then
     begin
      {Lock Filter}
      if ALock then if AState = NETWORK_LOCK_READ then Filter.ReaderLock else Filter.WriterLock;
      
      {Return Result}
      Result:=Filter;
     end;
   end
  else
   begin
    {Get Next}
    Filter:=TTransportFilter(APrevious.Next);
    if Filter <> nil then
     begin
      {Lock Filter}
      if ALock then if AState = NETWORK_LOCK_READ then Filter.ReaderLock else Filter.WriterLock;
      
      {Return Result}
      Result:=Filter;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FFilters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.GetConfigByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TTransportConfig;
var
 Config:TTransportConfig;
begin
 {}
 FConfigs.ReaderLock;
 try
  Result:=nil;
  
  {Get Config}
  Config:=TTransportConfig(FConfigs.First);
  while Config <> nil do
   begin
    {Check Config}
    if THandle(Config) = AHandle then
     begin
      {Lock Config} 
      if ALock then if AState = NETWORK_LOCK_READ then Config.ReaderLock else Config.WriterLock;
      
      {Return Result}
      Result:=Config;
      Exit;
     end;
     
    {Get Next} 
    Config:=TTransportConfig(Config.Next);
   end;
 finally 
  FConfigs.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.GetConfigByType(AConfigType:Word;ALock:Boolean;AState:LongWord):TTransportConfig;
var
 Config:TTransportConfig;
begin
 {}
 FConfigs.ReaderLock;
 try
  Result:=nil;
  
  {Get Config}
  Config:=TTransportConfig(FConfigs.First);
  while Config <> nil do
   begin
    {Check Config}
    if Config.ConfigType = AConfigType then
     begin
      {Lock Config} 
      if ALock then if AState = NETWORK_LOCK_READ then Config.ReaderLock else Config.WriterLock;
      
      {Return Result}
      Result:=Config;
      Exit;
     end;
     
    {Get Next} 
    Config:=TTransportConfig(Config.Next);
   end;
 finally 
  FConfigs.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.GetConfigByConfig(AConfig:TTransportConfig;ALock:Boolean;AState:LongWord):TTransportConfig;
var
 Config:TTransportConfig;
begin
 {}
 FConfigs.ReaderLock;
 try
  Result:=nil;
  
  {Get Config}
  Config:=TTransportConfig(FConfigs.First);
  while Config <> nil do
   begin
    {Check Config}
    if Config = AConfig then
     begin
      {Lock Config} 
      if ALock then if AState = NETWORK_LOCK_READ then Config.ReaderLock else Config.WriterLock;
      
      {Return Result}
      Result:=Config;
      Exit;
     end;
     
    {Get Next} 
    Config:=TTransportConfig(Config.Next);
   end;
 finally 
  FConfigs.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.GetConfigByNext(APrevious:TTransportConfig;ALock,AUnlock:Boolean;AState:LongWord):TTransportConfig;
var
 Config:TTransportConfig;
begin
 {}
 FConfigs.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Config:=TTransportConfig(FConfigs.First);
    if Config <> nil then
     begin
      {Lock Config}
      if ALock then if AState = NETWORK_LOCK_READ then Config.ReaderLock else Config.WriterLock;
      
      {Return Result}
      Result:=Config;
     end;
   end
  else
   begin
    {Get Next}
    Config:=TTransportConfig(APrevious.Next);
    if Config <> nil then
     begin
      {Lock Config}
      if ALock then if AState = NETWORK_LOCK_READ then Config.ReaderLock else Config.WriterLock;
      
      {Return Result}
      Result:=Config;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FConfigs.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkTransport.AddAdapter(AAdapter:TNetworkAdapter;AConfigType:Word;AAddress,ANetmask,AGateway,AServer:Pointer):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkTransport.RemoveAdapter(AAdapter:TNetworkAdapter):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkTransport.AddProtocol(AProtocol:Word;APacketHandler:TTransportPacketHandler;AControlHandler:TTransportControlHandler):THandle;
begin
 {Virtual Base Method}
 Result:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}

function TNetworkTransport.RemoveProtocol(AHandle:THandle;AProtocol:Word):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkTransport.AddFilter(AProtocol:Word;AFilterHandler:TTransportFilterHandler):THandle;
begin
 {Virtual Base Method}
 Result:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}

function TNetworkTransport.RemoveFilter(AHandle:THandle;AProtocol:Word):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkTransport.AddConfig(AConfigType:Word;AConfigAuto:Boolean;AConfigHandler:TTransportConfigHandler):THandle;
begin
 {Virtual Base Method}
 Result:=INVALID_HANDLE_VALUE;
end;

{==============================================================================}

function TNetworkTransport.RemoveConfig(AHandle:THandle;AConfigType:Word):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkTransport.SendPacket(ASocket:TTransportSocket;ASource,ADest:Pointer;APacket:PPacketFragment;ASize,AFlags:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkTransport.SendControl(ASource,ADest:Pointer;AProtocol,ACommand,ACode:Word;AAddress,AData:Pointer;ASize:Integer):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkTransport.FilterPacket(ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkTransport.ForwardPacket(AAdapter:TTransportAdapter;ASource,ADest,APacket:Pointer;ASize:Integer;ABroadcast:Boolean):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkTransport.GetSockOpt(ASocket:TTransportSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkTransport.SetSockOpt(ASocket:TTransportSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkTransport.GetStatistics:TTransportStatistics;
begin
 {Virtual Base Method}
 FillChar(Result,SizeOf(TTransportStatistics),0);
end;

{==============================================================================}

function TNetworkTransport.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkTransport.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkTransport.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkTransport.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkTransport.StartTransport:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkTransport.StopTransport:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkTransport.ProcessTransport:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkTransport.BindTransport(AAdapter:TNetworkAdapter):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkTransport.UnbindTransport(AAdapter:TNetworkAdapter):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}
{==============================================================================}
{TMonitorAdapter}
constructor TMonitorAdapter.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;

 Handle:=INVALID_HANDLE_VALUE;
 Adapter:=nil;
end;
 
{==============================================================================}

destructor TMonitorAdapter.Destroy; 
begin
 {}
 WriterLock;
 try
  Adapter:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TMonitorAdapter.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TMonitorAdapter.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TMonitorAdapter.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TMonitorAdapter.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TNetworkMonitor}
constructor TNetworkMonitor.Create(AManager:TTransportManager);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 FManager:=AManager;

 FAdapters:=TNetworkList.Create;
 if FManager <> nil then FManager.AddMonitor(Self);
end;
 
{==============================================================================}

destructor TNetworkMonitor.Destroy; 
begin
 {}
 WriterLock;
 try
  if FManager <> nil then FManager.RemoveMonitor(Self);
  FAdapters.Free;
  FManager:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TNetworkMonitor.GetAdapterByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TMonitorAdapter;
var
 Adapter:TMonitorAdapter;
begin
 {}
 FAdapters.ReaderLock;
 try
  Result:=nil;
  
  {Get Adapter}
  Adapter:=TMonitorAdapter(FAdapters.First);
  while Adapter <> nil do
   begin
    {Check Adapter}
    if Adapter.Handle = AHandle then
     begin
      {Lock Adapter} 
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
      Exit;
     end;
     
    {Get Next} 
    Adapter:=TMonitorAdapter(Adapter.Next);
   end;
 finally 
  FAdapters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkMonitor.GetAdapterByAdapter(AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TMonitorAdapter;
var
 Adapter:TMonitorAdapter;
begin
 {}
 FAdapters.ReaderLock;
 try
  Result:=nil;
  
  {Get Adapter}
  Adapter:=TMonitorAdapter(FAdapters.First);
  while Adapter <> nil do
   begin
    {Check Adapter}
    if Adapter.Adapter = AAdapter then
     begin
      {Lock Adapter} 
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
      Exit;
     end;
     
    {Get Next} 
    Adapter:=TMonitorAdapter(Adapter.Next);
   end;
 finally 
  FAdapters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkMonitor.GetAdapterByNext(APrevious:TMonitorAdapter;ALock,AUnlock:Boolean;AState:LongWord):TMonitorAdapter;
var
 Adapter:TMonitorAdapter;
begin
 {}
 FAdapters.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Adapter:=TMonitorAdapter(FAdapters.First);
    if Adapter <> nil then
     begin
      {Lock Adapter}
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
     end;
   end
  else
   begin
    {Get Next}
    Adapter:=TMonitorAdapter(APrevious.Next);
    if Adapter <> nil then
     begin
      {Lock Adapter}
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FAdapters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkMonitor.AddAdapter(AAdapter:TNetworkAdapter):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkMonitor.RemoveAdapter(AAdapter:TNetworkAdapter):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkMonitor.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkMonitor.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkMonitor.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkMonitor.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkMonitor.StartMonitor:Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkMonitor.StopMonitor:Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkMonitor.ProcessMonitor:Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkMonitor.BindMonitor(AAdapter:TNetworkAdapter):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkMonitor.UnbindMonitor(AAdapter:TNetworkAdapter):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}
{==============================================================================}
{TAuthenticatorAdapter}
constructor TAuthenticatorAdapter.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;

 Handle:=INVALID_HANDLE_VALUE;
 Adapter:=nil;
end;

{==============================================================================}

destructor TAuthenticatorAdapter.Destroy; 
begin
 {}
 WriterLock;
 try
  Adapter:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TAuthenticatorAdapter.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAuthenticatorAdapter.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAuthenticatorAdapter.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAuthenticatorAdapter.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TNetworkAuthenticator}
constructor TNetworkAuthenticator.Create(AManager:TTransportManager);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 FManager:=AManager;
 FAuthType:=AUTH_TYPE_UNKNOWN;
 FInitDelay:=0;
 FRetryCount:=1;
 FRetryTimeout:=5000;
 FAdapters:=TNetworkList.Create;
 if FManager <> nil then FManager.AddAuthenticator(Self);
end;

{==============================================================================}

destructor TNetworkAuthenticator.Destroy; 
begin
 {}
 WriterLock;
 try
  if FManager <> nil then FManager.RemoveAuthenticator(Self);
  FAdapters.Free;
  FManager:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TNetworkAuthenticator.GetAdapterByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TAuthenticatorAdapter;
var
 Adapter:TAuthenticatorAdapter;
begin
 {}
 FAdapters.ReaderLock;
 try
  Result:=nil;
  
  {Get Adapter}
  Adapter:=TAuthenticatorAdapter(FAdapters.First);
  while Adapter <> nil do
   begin
    {Check Adapter}
    if Adapter.Handle = AHandle then
     begin
      {Lock Adapter} 
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
      Exit;
     end;
     
    {Get Next} 
    Adapter:=TAuthenticatorAdapter(Adapter.Next);
   end;
 finally 
  FAdapters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAuthenticator.GetAdapterByAdapter(AAdapter:TNetworkAdapter;ALock:Boolean;AState:LongWord):TAuthenticatorAdapter;
var
 Adapter:TAuthenticatorAdapter;
begin
 {}
 FAdapters.ReaderLock;
 try
  Result:=nil;
  
  {Get Adapter}
  Adapter:=TAuthenticatorAdapter(FAdapters.First);
  while Adapter <> nil do
   begin
    {Check Adapter}
    if Adapter.Adapter = AAdapter then
     begin
      {Lock Adapter} 
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
      Exit;
     end;
     
    {Get Next} 
    Adapter:=TAuthenticatorAdapter(Adapter.Next);
   end;
 finally 
  FAdapters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAuthenticator.GetAdapterByNext(APrevious:TAuthenticatorAdapter;ALock,AUnlock:Boolean;AState:LongWord):TAuthenticatorAdapter;
var
 Adapter:TAuthenticatorAdapter;
begin
 {}
 FAdapters.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Adapter:=TAuthenticatorAdapter(FAdapters.First);
    if Adapter <> nil then
     begin
      {Lock Adapter}
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
     end;
   end
  else
   begin
    {Get Next}
    Adapter:=TAuthenticatorAdapter(APrevious.Next);
    if Adapter <> nil then
     begin
      {Lock Adapter}
      if ALock then if AState = NETWORK_LOCK_READ then Adapter.ReaderLock else Adapter.WriterLock;
      
      {Return Result}
      Result:=Adapter;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FAdapters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkAuthenticator.AddAdapter(AAdapter:TNetworkAdapter;AAuthType:Word;ACipher,AKey,AEntity,AToken:Pointer):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAuthenticator.RemoveAdapter(AAdapter:TNetworkAdapter):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAuthenticator.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkAuthenticator.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkAuthenticator.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkAuthenticator.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkAuthenticator.StartAuthenticator:Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAuthenticator.StopAuthenticator:Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAuthenticator.ProcessAuthenticator:Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAuthenticator.BindAuthenticator(AAdapter:TNetworkAdapter):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkAuthenticator.UnbindAuthenticator(AAdapter:TNetworkAdapter):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}
{==============================================================================}
{TTransportSocket}
constructor TTransportSocket.Create(ATransport:TNetworkTransport);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 FLocalLock:=MutexCreate;
 
 FTransport:=ATransport;

 {Set Address Family}
 FFamily:=AF_UNSPEC;
 if FTransport <> nil then FFamily:=FTransport.Family;
 FStruct:=SOCK_RAW;
 FProto:=IPPROTO_IP;
 FOwner:=nil;

 FSocketError:=ERROR_SUCCESS;
 {Create Socket State}
 FSocketState:=TSocketState.Create;
 {Create Socket Options}
 FSocketOptions:=TSocketOptions.Create;
 FOpenTime:=0;
 FCloseTime:=0;
 FLingerTime:=0;
 FTimewaitTime:=0;
 FKeepAliveTime:=0;

 FTransportState:=nil;
 FTransportOptions:=nil;
end;

{==============================================================================}

destructor TTransportSocket.Destroy;
begin
 {}
 WriterLock;
 try
  FTransportOptions:=nil;
  FTransportState:=nil; 

  {Free Socket Options}
  FSocketOptions.Free;
  {Free Socket State}
  FSocketState.Free;

  //To Do //LocalLock to protect Owner ?
  {Remove Owner}
  if FOwner <> nil then FOwner.Remove(Self);
  FOwner:=nil;
 
  FTransport:=nil;
  
  MutexDestroy(FLocalLock);
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

procedure TTransportSocket.SetFamily(AFamily:Word);
begin
 {}
 if not AcquireLock then Exit;

 FFamily:=AFamily;

 ReleaseLock;
end;

{==============================================================================}

procedure TTransportSocket.SetStruct(AStruct:Word);
begin
 {}
 if not AcquireLock then Exit;

 FStruct:=AStruct;

 ReleaseLock;
end;

{==============================================================================}

procedure TTransportSocket.SetProto(AProto:Word); 
begin
 {}
 if not AcquireLock then Exit;

 FProto:=AProto;

 ReleaseLock;
end;

{==============================================================================}

procedure TTransportSocket.SetOwner(AOwner:TSocketList);
begin
 {}
 if not AcquireLock then Exit;

 FOwner:=AOwner;

 ReleaseLock;
end;

{==============================================================================}

procedure TTransportSocket.SetSocketError(ASocketError:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FSocketError:=ASocketError;

 ReleaseLock;
end;

{==============================================================================}

procedure TTransportSocket.SetOpenTime(const AOpenTime:Int64);  
begin
 {}
 if not AcquireLock then Exit;

 FOpenTime:=AOpenTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TTransportSocket.SetCloseTime(const ACloseTime:Int64);  
begin
 {}
 if not AcquireLock then Exit;

 FCloseTime:=ACloseTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TTransportSocket.SetLingerTime(const ALingerTime:Int64); 
begin
 {}
 if not AcquireLock then Exit;

 FLingerTime:=ALingerTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TTransportSocket.SetTimewaitTime(const ATimewaitTime:Int64); 
begin
 {}
 if not AcquireLock then Exit;

 FTimewaitTime:=ATimewaitTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TTransportSocket.SetKeepAliveTime(const AKeepAliveTime:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FKeepAliveTime:=AKeepAliveTime;

 ReleaseLock;
end;

{==============================================================================}

function TTransportSocket.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportSocket.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportSocket.GetOption(ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer;
{Note: With Socket Options that are Boolean 0 is False and > 0 is True}
begin
 {}
 ReaderLock;
 try
  Result:=SOCKET_ERROR;
  
  {Check Level}
  case ALevel of
   SOL_SOCKET:begin
     SetLastError(WSAENOPROTOOPT);
     
     {Check Option}
     case AOptName of
      SO_DEBUG,SO_ACCEPTCONN,SO_REUSEADDR,SO_KEEPALIVE,SO_DONTROUTE,
      SO_BROADCAST,SO_USELOOPBACK,SO_OOBINLINE,SO_DONTLINGER,SO_SNDBUF,
      SO_RCVBUF,SO_SNDLOWAT,SO_RCVLOWAT,SO_SNDTIMEO,SO_RCVTIMEO,SO_CONNTIMEO:begin
        SetLastError(WSAEFAULT);
        
        if AOptLength >= SizeOf(Integer) then  {All these options a 4 bytes}
         begin
          Result:=NO_ERROR;
          
          {Setup Return}
          PInteger(AOptValue)^:=0;
          AOptLength:=SizeOf(Integer);
          
          case AOptName of
           SO_DEBUG:begin
             if SocketOptions.Debug then PInteger(AOptValue)^:=1;
            end;
           SO_ACCEPTCONN:begin
             if SocketOptions.Accept then PInteger(AOptValue)^:=1;
            end;
           SO_REUSEADDR:begin
             if SocketOptions.ReuseAddress then PInteger(AOptValue)^:=1;
            end;
           SO_KEEPALIVE:begin
             if SocketOptions.KeepAlive then PInteger(AOptValue)^:=1;
            end;
           SO_DONTROUTE:begin
             if SocketOptions.DontRoute then PInteger(AOptValue)^:=1;
            end;
           SO_BROADCAST:begin
             if SocketOptions.Broadcast then PInteger(AOptValue)^:=1;
            end;
           SO_USELOOPBACK:begin
             if SocketOptions.UseLoopback then PInteger(AOptValue)^:=1;
            end;
           SO_OOBINLINE:begin
             if SocketOptions.UrgentInline then PInteger(AOptValue)^:=1;
            end;
           SO_DONTLINGER:begin
             if SocketOptions.Linger.l_onoff = 0 then PInteger(AOptValue)^:=1;
            end;
           SO_SNDBUF:begin
             PInteger(AOptValue)^:=SocketOptions.SendBuffer;
            end;
           SO_RCVBUF:begin
             PInteger(AOptValue)^:=SocketOptions.RecvBuffer;
            end;
           SO_SNDLOWAT:begin
             PInteger(AOptValue)^:=SocketOptions.SendLowMark;
            end;
           SO_RCVLOWAT:begin
             PInteger(AOptValue)^:=SocketOptions.RecvLowMark;
            end;
           SO_SNDTIMEO:begin
             PInteger(AOptValue)^:=SocketOptions.SendTimeout;
            end;
           SO_RCVTIMEO:begin
             PInteger(AOptValue)^:=SocketOptions.RecvTimeout;
            end;
           SO_CONNTIMEO:begin
             PInteger(AOptValue)^:=SocketOptions.ConnTimeout;
            end;
          end;
         end;
       end;
      SO_ERROR:begin
        SetLastError(WSAEFAULT);
        
        if AOptLength >= SizeOf(Integer) then
         begin
          AOptLength:=SizeOf(Integer);
          PInteger(AOptValue)^:=SocketError;
          
          SocketError:=ERROR_SUCCESS;
          
          Result:=NO_ERROR;
         end;
       end;
      SO_TYPE:begin
        SetLastError(WSAEFAULT);
        
        if AOptLength >= SizeOf(Integer) then
         begin
          AOptLength:=SizeOf(Integer);
          PInteger(AOptValue)^:=Struct;
          
          Result:=NO_ERROR;
         end;
       end;
      SO_LINGER:begin
        SetLastError(WSAEFAULT);
        
        if AOptLength >= SizeOf(TLinger) then
         begin
          AOptLength:=SizeOf(TLinger);
          PLinger(AOptValue)^:=SocketOptions.Linger;
          
          Result:=NO_ERROR;
         end;
       end;
     end;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;


{==============================================================================}

function TTransportSocket.SetOption(ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer;
{Note: With Socket Options that are Boolean 0 is False and > 0 is True}
var
 Linger:TLinger;
begin
 {}
 ReaderLock;
 try
  Result:=SOCKET_ERROR;
  
  {Check Level}
  case ALevel of
   SOL_SOCKET:begin
     SetLastError(WSAENOPROTOOPT);
     
     {Check Option}
     case AOptName of
      SO_DEBUG,SO_ACCEPTCONN,SO_REUSEADDR,SO_KEEPALIVE,SO_DONTROUTE,
      SO_BROADCAST,SO_USELOOPBACK,SO_OOBINLINE,SO_DONTLINGER,SO_SNDBUF,
      SO_RCVBUF,SO_SNDLOWAT,SO_RCVLOWAT,SO_SNDTIMEO,SO_RCVTIMEO,SO_CONNTIMEO:begin
        SetLastError(WSAEFAULT);
        
        if AOptLength >= SizeOf(Integer) then
         begin
          Result:=NO_ERROR;
          
          case AOptName of
           SO_DEBUG:begin
             SocketOptions.Debug:=(PInteger(AOptValue)^ <> 0);
            end;
           SO_ACCEPTCONN:begin
             SocketOptions.Accept:=(PInteger(AOptValue)^ <> 0);
            end;
           SO_REUSEADDR:begin
             SocketOptions.ReuseAddress:=(PInteger(AOptValue)^ <> 0);
            end;
           SO_KEEPALIVE:begin
             SocketOptions.KeepAlive:=(PInteger(AOptValue)^ <> 0);
            end;
           SO_DONTROUTE:begin
             SocketOptions.DontRoute:=(PInteger(AOptValue)^ <> 0);
            end;
           SO_BROADCAST:begin
             SocketOptions.Broadcast:=(PInteger(AOptValue)^ <> 0);
             SocketState.Privileged:=(PInteger(AOptValue)^ <> 0);
            end;
           SO_USELOOPBACK:begin
             SocketOptions.UseLoopback:=(PInteger(AOptValue)^ <> 0);
            end;
           SO_OOBINLINE:begin
             SocketOptions.UrgentInline:=(PInteger(AOptValue)^ <> 0);
            end;
           SO_DONTLINGER:begin
             {Get Linger}
             Linger:=SocketOptions.Linger;
             
             {Set Option}
             if PInteger(AOptValue)^ <> 0 then
              Linger.l_onoff:=0   {Linger Off}
             else
              Linger.l_onoff:=1;  {Linger On}
              
             {Set Linger}
             SocketOptions.Linger:=Linger;
            end;
           SO_SNDBUF:begin
             SocketOptions.SendBuffer:=PInteger(AOptValue)^;
            end;
           SO_RCVBUF:begin
             SocketOptions.RecvBuffer:=PInteger(AOptValue)^;
            end;
           SO_SNDLOWAT:begin
             SocketOptions.SendLowMark:=PInteger(AOptValue)^;
            end;
           SO_RCVLOWAT:begin
             SocketOptions.RecvLowMark:=PInteger(AOptValue)^;
            end;
           SO_SNDTIMEO:begin
             SocketOptions.SendTimeout:=PInteger(AOptValue)^;
            end;
           SO_RCVTIMEO:begin
             SocketOptions.RecvTimeout:=PInteger(AOptValue)^;
            end;
           SO_CONNTIMEO:begin
             SocketOptions.ConnTimeout:=PInteger(AOptValue)^;
            end;
          end;
         end;
       end;
      SO_ERROR:begin
        SetLastError(WSAEFAULT);
        
        if AOptLength >= SizeOf(Integer) then
         begin
          SocketError:=PInteger(AOptValue)^;
          
          Result:=NO_ERROR;
         end;
       end;
      SO_TYPE:begin
        SetLastError(WSAEFAULT);
        
        if AOptLength >= SizeOf(Integer) then
         begin
          Struct:=PInteger(AOptValue)^;
          
          Result:=NO_ERROR;
         end;
       end;
      SO_LINGER:begin
        SetLastError(WSAEFAULT);
        
        if AOptLength >= SizeOf(TLinger) then
         begin
          SocketOptions.Linger:=PLinger(AOptValue)^;
          
          Result:=NO_ERROR;
         end;
       end;
     end;
    end;
  end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TTransportSocket.IoCtl(ACommand:DWORD;var AArgument:u_long):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TTransportSocket.IsConnected(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TTransportSocket.IsListening(ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast:Boolean):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TTransportSocket.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportSocket.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportSocket.ReaderConvert:Boolean; 
{Convert a Reader lock to a Writer lock}
begin
 {}
 Result:=(SynchronizerReaderConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportSocket.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportSocket.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportSocket.WriterConvert:Boolean;
{Convert a Writer lock to a Reader lock}
begin
 {}
 Result:=(SynchronizerWriterConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TSocketList}
constructor TSocketList.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 FList:=TList.Create;
end;

{==============================================================================}

destructor TSocketList.Destroy;
begin
 {}
 AcquireLock;
 try
  RemoveListItems;
  
  FList.Free;
  inherited Destroy;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

procedure TSocketList.RemoveListItems;
var
 Count:Integer;
 Socket:TTransportSocket;
begin
 {}
 if not AcquireLock then Exit;
 try
  {Get Sockets}
  for Count:=FList.Count - 1 downto 0 do
   begin
    {Get Socket}
    Socket:=TTransportSocket(FList.Items[Count]);
    
    {Reset Owner}
    Socket.Owner:=nil;
    
    {Remove Socket}
    FList.Remove(Socket);
   end;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSocketList.GetCount:Integer;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;
 try
  {Get Socket Count}
  Result:=FList.Count;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSocketList.GetItem(AIndex:Integer):TTransportSocket;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {Get Socket Item}
  Result:=FList.Items[AIndex];
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TSocketList.SetItem(AIndex:Integer;AItem:TTransportSocket);
begin
 {}
 if not AcquireLock then Exit;
 try
  {Set Socket Item}
  FList.Items[AIndex]:=AItem;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSocketList.Add(ASocket:TTransportSocket):Integer;
begin
 {}
 Result:=-1;
 
 if not AcquireLock then Exit;
 try
  {Check Socket}
  if ASocket = nil then Exit;
  
  {Set Owner}
  ASocket.Owner:=Self;
  
  {Add Socket}
  Result:=FList.Add(ASocket);
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSocketList.Remove(ASocket:TTransportSocket):Integer;
begin
 {}
 Result:=-1;
 
 if not AcquireLock then Exit;
 try
  {Check Socket}
  if ASocket = nil then Exit;
  
  {Reset Owner}
  ASocket.Owner:=nil;
  
  {Remove Socket}
  Result:=FList.Remove(ASocket);
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSocketList.First:TTransportSocket;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {Get First Socket}
  Result:=FList.First;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSocketList.Last:TTransportSocket;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  {Get Last Socket}
  Result:=FList.Last;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

procedure TSocketList.ClearList;
begin
 {}
 if not AcquireLock then Exit;
 try
  {Remove Sockets}
  RemoveListItems;

  {Clear List}
  FList.Clear;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSocketList.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TSocketList.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TSocketState}
constructor TSocketState.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FState:=SS_UNCONNECTED;
end;

{==============================================================================}

destructor TSocketState.Destroy;
begin
 {}
 AcquireLock;
 try
  inherited Destroy;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TSocketState.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TSocketState.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TSocketState.GetUnconnected:Boolean;
begin
 {}
 Result:=(FState and SS_UNCONNECTED) = SS_UNCONNECTED;
end;

{==============================================================================}

procedure TSocketState.SetUnconnected(AUnconnected:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if AUnconnected then
  begin
   FState:=FState or SS_UNCONNECTED;
   FState:=FState and not(SS_ISCONNECTED);
   FState:=FState and not(SS_ISCONNECTING);
   FState:=FState and not(SS_ISDISCONNECTING);
  end
 else
  begin
   FState:=FState and not(SS_UNCONNECTED);
  end;
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetConnected:Boolean;
begin
 {}
 Result:=(FState and SS_ISCONNECTED) = SS_ISCONNECTED;
end;

{==============================================================================}

procedure TSocketState.SetConnected(AConnected:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if AConnected then
  begin
   FState:=FState or SS_ISCONNECTED;
   FState:=FState and not(SS_UNCONNECTED);
   FState:=FState and not(SS_ISCONNECTING);
   FState:=FState and not(SS_ISDISCONNECTING);
  end
 else
  begin
   FState:=FState and not(SS_ISCONNECTED);
  end;
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetConnecting:Boolean;
begin
 {}
 Result:=(FState and SS_ISCONNECTING) = SS_ISCONNECTING;
end;

{==============================================================================}

procedure TSocketState.SetConnecting(AConnecting:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if AConnecting then
  begin
   FState:=FState or SS_ISCONNECTING;
   FState:=FState and not(SS_UNCONNECTED);
   FState:=FState and not(SS_ISCONNECTED);
   FState:=FState and not(SS_ISDISCONNECTING);
  end
 else
  begin
   FState:=FState and not(SS_ISCONNECTING);
  end;
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetDisconnecting:Boolean;
begin
 {}
 Result:=(FState and SS_ISDISCONNECTING) = SS_ISDISCONNECTING;
end;

{==============================================================================}

procedure TSocketState.SetDisconnecting(ADisconnecting:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if ADisconnecting then
  begin
   FState:=FState or SS_ISDISCONNECTING;
   FState:=FState and not(SS_UNCONNECTED);
   FState:=FState and not(SS_ISCONNECTED);
   FState:=FState and not(SS_ISCONNECTING);
  end
 else
  begin
   FState:=FState and not(SS_ISDISCONNECTING);
  end;
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetCantSendMore:Boolean;
begin
 {}
 Result:=(FState and SS_CANTSENDMORE) = SS_CANTSENDMORE;
end;

{==============================================================================}

procedure TSocketState.SetCantSendMore(ACantSendMore:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if ACantSendMore then
  FState:=FState or SS_CANTSENDMORE
 else
  FState:=FState and not(SS_CANTSENDMORE);
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetCantRecvMore:Boolean;
begin
 {}
 Result:=(FState and SS_CANTRCVMORE) = SS_CANTRCVMORE;
end;

{==============================================================================}

procedure TSocketState.SetCantRecvMore(ACantRecvMore:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if ACantRecvMore then
  FState:=FState or SS_CANTRCVMORE
 else
  FState:=FState and not(SS_CANTRCVMORE);
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetRecvAtMark:Boolean;
begin
 {}
 Result:=(FState and SS_RCVATMARK) = SS_RCVATMARK;
end;

{==============================================================================}

procedure TSocketState.SetRecvAtMark(ARecvAtMark:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if ARecvAtMark then
  FState:=FState or SS_RCVATMARK
 else
  FState:=FState and not(SS_RCVATMARK);
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetPrivileged:Boolean;
begin
 {}
 Result:=(FState and SS_PRIV) = SS_PRIV;
end;

{==============================================================================}

procedure TSocketState.SetPrivileged(APrivileged:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if APrivileged then
  FState:=FState or SS_PRIV
 else
  FState:=FState and not(SS_PRIV);
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetNonBlocking:Boolean;
begin
 {}
 Result:=(FState and SS_NBIO) = SS_NBIO;
end;

{==============================================================================}

procedure TSocketState.SetNonBlocking(ANonBlocking:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if ANonBlocking then
  FState:=FState or SS_NBIO
 else
  FState:=FState and not(SS_NBIO);
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetAsync:Boolean;
begin
 {}
 Result:=(FState and SS_ASYNC) = SS_ASYNC;
end;

{==============================================================================}

procedure TSocketState.SetAsync(AAsync:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if AAsync then
  FState:=FState or SS_ASYNC
 else
  FState:=FState and not(SS_ASYNC);
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetListening:Boolean;
begin
 {}
 Result:=(FState and SS_ISLISTENING) = SS_ISLISTENING;
end;

{==============================================================================}

procedure TSocketState.SetListening(AListening:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if AListening then
  FState:=FState or SS_ISLISTENING
 else
  FState:=FState and not(SS_ISLISTENING);
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetLocalAddress:Boolean;
begin
 {}
 Result:=(FState and SS_LOCAL_ADDR) = SS_LOCAL_ADDR;
end;

{==============================================================================}

procedure TSocketState.SetLocalAddress(ALocalAddress:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if ALocalAddress then
  FState:=FState or SS_LOCAL_ADDR
 else
  FState:=FState and not(SS_LOCAL_ADDR);
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetRemoteAddress:Boolean;
begin
 {}
 Result:=(FState and SS_REMOTE_ADDR) = SS_REMOTE_ADDR;
end;

{==============================================================================}

procedure TSocketState.SetRemoteAddress(ARemoteAddress:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if ARemoteAddress then
  FState:=FState or SS_REMOTE_ADDR
 else
  FState:=FState and not(SS_REMOTE_ADDR);
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetConnRefused:Boolean;
begin
 {}
 Result:=(FState and SS_CONN_REFUSED) = SS_CONN_REFUSED;
end;

{==============================================================================}

procedure TSocketState.SetConnRefused(AConnRefused:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if AConnRefused then
  begin
   FState:=FState or SS_CONN_REFUSED;
   SetUnconnected(True);
  end
 else
  begin
   FState:=FState and not(SS_CONN_REFUSED);
  end;
  
 ReleaseLock;
end;

{==============================================================================}

function TSocketState.GetClosed:Boolean;
begin
 {}
 Result:=(FState and SS_CLOSED) = SS_CLOSED;
end;

{==============================================================================}

procedure TSocketState.SetClosed(AClosed:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if AClosed then
  begin
   FState:=FState or SS_CLOSED;
   SetUnconnected(True);
  end
 else
  begin
   FState:=FState and not(SS_CLOSED);
  end;
  
 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TSocketBuffer}
constructor TSocketBuffer.Create(ASocket:TTransportSocket);
begin
 {}
 inherited Create;
 FLock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE); //CriticalSectionCreate; //TestingRPi
 
 FSocket:=ASocket;
 FBuffer:=TMemoryStream.Create;

 FSize:=0;
 FStart:=nil;
 FEnd:=nil;

 FUsed:=0;
 FFree:=FSize;
end;

{==============================================================================}

destructor TSocketBuffer.Destroy;
begin
 {}
 AcquireLock;
 try
  FBuffer.Free;
  FSocket:=nil;
  inherited Destroy;
 finally 
  ReleaseLock; {Cannot destroy Mutex while holding lock}  //{ReleaseLock;} {Can destroy Critical Section while holding lock}  //TestingRPi
  MutexDestroy(FLock); //CriticalSectionDestroy(FLock); //TestingRPi
 end;
end;

{==============================================================================}

function TSocketBuffer.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLock) = ERROR_SUCCESS); //Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS); //TestingRPi
end;

{==============================================================================}

function TSocketBuffer.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLock) = ERROR_SUCCESS); //Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS); //TestingRPi
end;

{==============================================================================}

procedure TSocketBuffer.SetSize(ASize:LongWord);
begin
 {Virtual Base Method}
end;

{==============================================================================}

function TSocketBuffer.GetUsed:LongWord;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FUsed;

 ReleaseLock;
end;

{==============================================================================}

function TSocketBuffer.GetFree:LongWord;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FFree;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TSocketOptions}
constructor TSocketOptions.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FOptions:=0;
 
 FLinger.l_onoff:=0;
 FLinger.l_linger:=LINGER_TIMEOUT;

 FSendTimeout:=SEND_TIMEOUT;
 FRecvTimeout:=RECV_TIMEOUT;
 FConnTimeout:=CONNECT_TIMEOUT;
end;

{==============================================================================}

destructor TSocketOptions.Destroy;
begin
 {}
 AcquireLock;
 try
  inherited Destroy;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TSocketOptions.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TSocketOptions.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

procedure TSocketOptions.SetLinger(const ALinger:TLinger);
begin
 {}
 if not AcquireLock then Exit;

 FLinger:=ALinger;

 ReleaseLock;
end;
 
{==============================================================================}

procedure TSocketOptions.SetSendBuffer(ASendBuffer:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FSendBuffer:=ASendBuffer;

 ReleaseLock;
end;

{==============================================================================}

procedure TSocketOptions.SetRecvBuffer(ARecvBuffer:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FRecvBuffer:=ARecvBuffer;

 ReleaseLock;
end;

{==============================================================================}

procedure TSocketOptions.SetSendLowMark(ASendLowMark:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FSendLowMark:=ASendLowMark;

 ReleaseLock;
end;

{==============================================================================}

procedure TSocketOptions.SetRecvLowMark(ARecvLowMark:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FRecvLowMark:=ARecvLowMark;

 ReleaseLock;
end;

{==============================================================================}

procedure TSocketOptions.SetSendTimeout(ASendTimeout:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FSendTimeout:=ASendTimeout;

 ReleaseLock;
end;

{==============================================================================}

procedure TSocketOptions.SetRecvTimeout(ARecvTimeout:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FRecvTimeout:=ARecvTimeout;

 ReleaseLock;
end;

{==============================================================================}

procedure TSocketOptions.SetConnTimeout(AConnTimeout:LongWord);
begin
 {}
 if not AcquireLock then Exit;

 FConnTimeout:=AConnTimeout;

 ReleaseLock;
end;

{==============================================================================}

function TSocketOptions.GetDebug:Boolean;
begin
 {}
 Result:=(FOptions and SO_DEBUG) = SO_DEBUG;
end;

{==============================================================================}

procedure TSocketOptions.SetDebug(ADebug:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if ADebug then
  FOptions:=FOptions or SO_DEBUG
 else
  FOptions:=FOptions and not(SO_DEBUG);

 ReleaseLock;
end;

{==============================================================================}

function TSocketOptions.GetAccept:Boolean;
begin
 {}
 Result:=(FOptions and SO_ACCEPTCONN) = SO_ACCEPTCONN;
end;

{==============================================================================}

procedure TSocketOptions.SetAccept(AAccept:Boolean);
begin
 {}
 if not AcquireLock then Exit;
 
 if AAccept then
  FOptions:=FOptions or SO_ACCEPTCONN
 else
  FOptions:=FOptions and not(SO_ACCEPTCONN);

 ReleaseLock;
end;

{==============================================================================}

function TSocketOptions.GetReuseAddress:Boolean;
begin
 {}
 Result:=(FOptions and SO_REUSEADDR) = SO_REUSEADDR;
end;

{==============================================================================}

procedure TSocketOptions.SetReuseAddress(AReuseAddress:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AReuseAddress then
  FOptions:=FOptions or SO_REUSEADDR
 else
  FOptions:=FOptions and not(SO_REUSEADDR);

 ReleaseLock;
end;

{==============================================================================}

function TSocketOptions.GetKeepAlive:Boolean;
begin
 {}
 Result:=(FOptions and SO_KEEPALIVE) = SO_KEEPALIVE;
end;

{==============================================================================}

procedure TSocketOptions.SetKeepAlive(AKeepAlive:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AKeepAlive then
  FOptions:=FOptions or SO_KEEPALIVE
 else
  FOptions:=FOptions and not(SO_KEEPALIVE);

 ReleaseLock;
end;

{==============================================================================}

function TSocketOptions.GetDontRoute:Boolean;
begin
 {}
 Result:=(FOptions and SO_DONTROUTE) = SO_DONTROUTE;
end;

{==============================================================================}

procedure TSocketOptions.SetDontRoute(ADontRoute:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if ADontRoute then
  FOptions:=FOptions or SO_DONTROUTE
 else
  FOptions:=FOptions and not(SO_DONTROUTE);

 ReleaseLock;
end;

{==============================================================================}

function TSocketOptions.GetBroadcast:Boolean;
begin
 {}
 Result:=(FOptions and SO_BROADCAST) = SO_BROADCAST;
end;

{==============================================================================}

procedure TSocketOptions.SetBroadcast(ABroadcast:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if ABroadcast then
  FOptions:=FOptions or SO_BROADCAST
 else
  FOptions:=FOptions and not(SO_BROADCAST);

 ReleaseLock;
end;

{==============================================================================}

function TSocketOptions.GetUseLoopback:Boolean;
begin
 {}
 Result:=(FOptions and SO_USELOOPBACK) = SO_USELOOPBACK;
end;

{==============================================================================}

procedure TSocketOptions.SetUseLoopback(AUseLoopback:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AUseLoopback then
  FOptions:=FOptions or SO_USELOOPBACK
 else
  FOptions:=FOptions and not(SO_USELOOPBACK);

 ReleaseLock;
end;

{==============================================================================}

function TSocketOptions.GetUrgentInline:Boolean;
begin
 {}
 Result:=(FOptions and SO_OOBINLINE) = SO_OOBINLINE;
end;

{==============================================================================}

procedure TSocketOptions.SetUrgentInline(AUrgentInline:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 if AUrgentInline then
  FOptions:=FOptions or SO_OOBINLINE
 else
  FOptions:=FOptions and not(SO_OOBINLINE);

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TTransportState}
constructor TTransportState.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
end;

{==============================================================================}

destructor TTransportState.Destroy;
begin
 {}
 AcquireLock;
 try
  inherited Destroy;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TTransportState.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportState.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TTransportOptions}
constructor TTransportOptions.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
end;

{==============================================================================}

destructor TTransportOptions.Destroy;
begin
 {}
 AcquireLock;
 try
  inherited Destroy;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TTransportOptions.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TTransportOptions.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{THostEntry}
constructor THostEntry.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FHostType:=HOST_TYPE_DYNAMIC;
 FHostTime:=GetTickCount64;
 
 FAliases:=TStringList.Create;
end;

{==============================================================================}

destructor THostEntry.Destroy;
begin
 {}
 AcquireLock;
 try
  FAliases.Free;
  inherited Destroy;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function THostEntry.GetName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FName;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure THostEntry.SetName(const AName:String);
begin
 {}
 if not AcquireLock then Exit;

 FName:=AName;
 UniqueString(FName);

 ReleaseLock;
end;

{==============================================================================}

procedure THostEntry.SetFamily(AFamily:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FFamily:=AFamily;

 ReleaseLock;
end;

{==============================================================================}

procedure THostEntry.SetLength(ALength:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FLength:=ALength;

 ReleaseLock;
end;

{==============================================================================}

procedure THostEntry.SetHostType(AHostType:Word);
begin
 {}
 if not AcquireLock then Exit;

 FHostType:=AHostType;

 ReleaseLock;
end;

{==============================================================================}

procedure THostEntry.SetHostTime(const AHostTime:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FHostTime:=AHostTime;

 ReleaseLock;
end;

{==============================================================================}

function THostEntry.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THostEntry.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function THostEntry.FindAlias(const Alias:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  Result:=(FAliases.IndexOf(Alias) <> -1);
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function THostEntry.AddAlias(const Alias:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  Result:=(FAliases.Add(Alias) <> -1);
 finally 
  ReleaseLock;
 end; 
end;
 
{==============================================================================}

function THostEntry.RemoveAlias(const Alias:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  FAliases.Delete(FAliases.IndexOf(Alias));
  
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TRouteEntry}
constructor TRouteEntry.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 FLocalLock:=MutexCreate;
 
 FMetric:=1;
 FRouteType:=ROUTE_TYPE_DYNAMIC;
 FRouteTime:=GetTickCount64;
end;

{==============================================================================}

destructor TRouteEntry.Destroy;
begin
 {}
 WriterLock;
 try
  MutexDestroy(FLocalLock);
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

procedure TRouteEntry.SetFamily(AFamily:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FFamily:=AFamily;

 ReleaseLock;
end;

{==============================================================================}

procedure TRouteEntry.SetLength(ALength:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FLength:=ALength;

 ReleaseLock;
end;

{==============================================================================}

procedure TRouteEntry.SetMetric(AMetric:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FMetric:=AMetric;

 ReleaseLock;
end;

{==============================================================================}

procedure TRouteEntry.SetRouteType(ARouteType:Word);
begin
 {}
 if not AcquireLock then Exit;

 FRouteType:=ARouteType;

 ReleaseLock;
end;

{==============================================================================}

procedure TRouteEntry.SetRouteTime(const ARouteTime:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FRouteTime:=ARouteTime;

 ReleaseLock;
end;

{==============================================================================}

function TRouteEntry.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TRouteEntry.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TRouteEntry.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TRouteEntry.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TRouteEntry.ReaderConvert:Boolean; 
{Convert a Reader lock to a Writer lock}
begin
 {}
 Result:=(SynchronizerReaderConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TRouteEntry.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TRouteEntry.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TRouteEntry.WriterConvert:Boolean;
{Convert a Writer lock to a Reader lock}
begin
 {}
 Result:=(SynchronizerWriterConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TAddressEntry}
constructor TAddressEntry.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 FLocalLock:=MutexCreate;
 
 FAddressType:=ADDRESS_TYPE_DYNAMIC;
 FAddressTime:=GetTickCount64;
 FAdapter:=nil;
end;

{==============================================================================}

destructor TAddressEntry.Destroy; 
begin
 {}
 WriterLock;
 try
  MutexDestroy(FLocalLock);
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

procedure TAddressEntry.SetFamily(AFamily:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FFamily:=AFamily;

 ReleaseLock;
end;

{==============================================================================}

procedure TAddressEntry.SetLength(ALength:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FLength:=ALength;

 ReleaseLock;
end;

{==============================================================================}

procedure TAddressEntry.SetAddressType(AAddressType:Word);
begin
 {}
 if not AcquireLock then Exit;

 FAddressType:=AAddressType;

 ReleaseLock;
end;

{==============================================================================}

procedure TAddressEntry.SetAddressTime(const AAddressTime:Int64);
begin
 {}
 if not AcquireLock then Exit;

 FAddressTime:=AAddressTime;

 ReleaseLock;
end;

{==============================================================================}

procedure TAddressEntry.SetAdapter(AAdapter:TNetworkAdapter);
begin
 {}
 if not AcquireLock then Exit;

 FAdapter:=AAdapter;

 ReleaseLock;
end;

{==============================================================================}

function TAddressEntry.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAddressEntry.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAddressEntry.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAddressEntry.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAddressEntry.ReaderConvert:Boolean; 
{Convert a Reader lock to a Writer lock}
begin
 {}
 Result:=(SynchronizerReaderConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAddressEntry.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAddressEntry.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TAddressEntry.WriterConvert:Boolean;
{Convert a Writer lock to a Reader lock}
begin
 {}
 Result:=(SynchronizerWriterConvert(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TNetworkEntry}
constructor TNetworkEntry.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FAliases:=TStringList.Create;
end;

{==============================================================================}

destructor TNetworkEntry.Destroy;
begin
 {}
 AcquireLock;
 try
  FAliases.Free;
  inherited Destroy;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TNetworkEntry.GetName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FName;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TNetworkEntry.SetName(const AName:String);
begin
 {}
 if not AcquireLock then Exit;

 FName:=AName;
 UniqueString(FName);

 ReleaseLock;
end;

{==============================================================================}

procedure TNetworkEntry.SetFamily(AFamily:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FFamily:=AFamily;

 ReleaseLock;
end;

{==============================================================================}

procedure TNetworkEntry.SetLength(ALength:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FLength:=ALength;

 ReleaseLock;
end;

{==============================================================================}

function TNetworkEntry.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkEntry.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkEntry.FindAlias(const Alias:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  Result:=(FAliases.IndexOf(Alias) <> -1);
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TNetworkEntry.AddAlias(const Alias:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  Result:=(FAliases.Add(Alias) <> -1);
 finally 
  ReleaseLock;
 end; 
end;
 
{==============================================================================}

function TNetworkEntry.RemoveAlias(const Alias:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  FAliases.Delete(FAliases.IndexOf(Alias));
  
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TServEntry}
constructor TServEntry.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FAliases:=TStringList.Create;
end;

{==============================================================================}

destructor TServEntry.Destroy;
begin
 {}
 AcquireLock;
 try
  FAliases.Free;
  inherited Destroy;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TServEntry.GetName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FName;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TServEntry.SetName(const AName:String);
begin
 {}
 if not AcquireLock then Exit;

 FName:=AName;
 UniqueString(FName);

 ReleaseLock;
end;

{==============================================================================}

procedure TServEntry.SetPort(APort:Word);
begin
 {}
 if not AcquireLock then Exit;

 FPort:=APort;

 ReleaseLock;
end;

{==============================================================================}

function TServEntry.GetProtocol:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FProtocol;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TServEntry.SetProtocol(const AProtocol:String);
begin
 {}
 if not AcquireLock then Exit;

 FProtocol:=AProtocol;
 UniqueString(FProtocol);

 ReleaseLock;
end;

{==============================================================================}

function TServEntry.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TServEntry.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TServEntry.FindAlias(const Alias:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  Result:=(FAliases.IndexOf(Alias) <> -1);
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TServEntry.AddAlias(const Alias:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  Result:=(FAliases.Add(Alias) <> -1);
 finally 
  ReleaseLock;
 end; 
end;
 
{==============================================================================}

function TServEntry.RemoveAlias(const Alias:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  FAliases.Delete(FAliases.IndexOf(Alias));
  
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TProtoEntry}
constructor TProtoEntry.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FAliases:=TStringList.Create;
end;

{==============================================================================}

destructor TProtoEntry.Destroy;
begin
 {}
 AcquireLock;
 try
  FAliases.Free;
  inherited Destroy;
 finally 
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TProtoEntry.GetName:String;
begin
 {}
 Result:='';
 
 if not AcquireLock then Exit;

 Result:=FName;
 UniqueString(Result);

 ReleaseLock;
end;

{==============================================================================}

procedure TProtoEntry.SetName(const AName:String);
begin
 {}
 if not AcquireLock then Exit;

 FName:=AName;
 UniqueString(FName);

 ReleaseLock;
end;

{==============================================================================}

procedure TProtoEntry.SetNumber(ANumber:Word);
begin
 {}
 if not AcquireLock then Exit;

 FNumber:=ANumber;

 ReleaseLock;
end;

{==============================================================================}

function TProtoEntry.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtoEntry.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtoEntry.FindAlias(const Alias:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  Result:=(FAliases.IndexOf(Alias) <> -1);
 finally 
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TProtoEntry.AddAlias(const Alias:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  Result:=(FAliases.Add(Alias) <> -1);
 finally 
  ReleaseLock;
 end; 
end;
 
{==============================================================================}

function TProtoEntry.RemoveAlias(const Alias:String):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  FAliases.Delete(FAliases.IndexOf(Alias));
  
  Result:=True;
 finally 
  ReleaseLock;
 end; 
end;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure TransportInit;
begin
 {}
 {Check Initialized}
 if TransportInitialized then Exit;

 {Create Transport Manager}
 TransportManager:=TTransportManager.Create(NetworkSettings,AdapterManager);
 
 TransportInitialized:=True;
end;

{==============================================================================}
 
function TransportStart:LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if TransportStarted then Exit;
 
 Result:=ERROR_INVALID_PARAMETER;

 {Check Manager}
 if TransportManager = nil then Exit;
 
 {Start Transports}
 if not TransportManager.StartTransports then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to start one or more network transports');
  end;
 
 {Start Monitors}
 if not TransportManager.StartMonitors then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to start one or more network monitors');
  end;

 {Start Authenticators}
 if not TransportManager.StartAuthenticators then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to start one or more network authenticators');
  end;
  
 {Set Started} 
 TransportStarted:=True;
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function TransportStop:LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if not(TransportStarted) then Exit;
 
 Result:=ERROR_INVALID_PARAMETER;

 {Check Manager}
 if TransportManager = nil then Exit;
 
 {Stop Authenticators}
 if not TransportManager.StopAuthenticators then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to stop one or more network authenticators');
  end;
 
 {Stop Monitors}
 if not TransportManager.StopMonitors then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to stop one or more network monitors');
  end;
 
 {Stop Transports}
 if not TransportManager.StopTransports then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to stop one or more network transports');
  end;

 {Set Started}
 TransportStarted:=False;    
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function TransportBind:LongWord;
begin
 {}
 Result:=ERROR_NOT_READY;
 
 {Check Started}
 if not(TransportStarted) then Exit;
 
 Result:=ERROR_INVALID_PARAMETER;

 {Check Manager}
 if TransportManager = nil then Exit;

 {Bind Transports}
 if not TransportManager.BindTransports(nil) then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to bind one or more network transports');
  end;
 
 {Bind Monitors}
 if not TransportManager.BindMonitors(nil) then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to bind one or more network monitors');
  end;

 {Bind Authenticators}
 if not TransportManager.BindAuthenticators(nil) then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to bind one or more network authenticators');
  end;
  
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function TransportUnbind:LongWord;
begin
 {}
 Result:=ERROR_NOT_READY;
 
 {Check Started}
 if not(TransportStarted) then Exit;
 
 Result:=ERROR_INVALID_PARAMETER;

 {Check Manager}
 if TransportManager = nil then Exit;

 {Unbind Authenticators}
 if not TransportManager.UnbindAuthenticators(nil) then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to unbind one or more network authenticators');
  end;
 
 {Unbind Monitors}
 if not TransportManager.UnbindMonitors(nil) then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to unbind one or more network monitors');
  end;
 
 {Unbind Transports}
 if not TransportManager.UnbindTransports(nil) then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to unbind one or more network transports');
  end;
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{Transport Functions}
function InAddrToHost(const AAddress:TInAddr):TInAddr; inline;
{Convert an InAddr in Network order to Host order}
begin
 {}
 Result.S_addr:=LongWordBEtoN(AAddress.S_addr); {Big Endian to Native}
end;

{==============================================================================}

function InAddrToNetwork(const AAddress:TInAddr):TInAddr; inline;
{Convert an InAddr in Host order to Network order}
begin
 {}
 Result.S_addr:=LongWordNtoBE(AAddress.S_addr); {Native to Big Endian}
end;

{==============================================================================}

function InAddrToString(const AAddress:TInAddr):String;
{Convert an InAddr to a String}
{Note: Expects Address to be in Network order}
begin
 {}
 Result:=IntToStr(Byte(AAddress.S_un_b.s_b1)) + '.'
       + IntToStr(Byte(AAddress.S_un_b.s_b2)) + '.'
       + IntToStr(Byte(AAddress.S_un_b.s_b3)) + '.'
       + IntToStr(Byte(AAddress.S_un_b.s_b4));
end;

{==============================================================================}

function StringToInAddr(const AAddress:String):TInAddr;
{Convert a String to an InAddr}
{Note: Returns Address in Network order}
var
 PosIdx:Integer;
 WorkBuffer:String;
begin
 {}
 LongWord(Result.S_addr):=INADDR_ANY;
 try
  WorkBuffer:=AAddress;
  
  {First Octet}
  PosIdx:=Pos('.',WorkBuffer);
  if PosIdx <> 0 then
   begin
    Byte(Result.S_un_b.s_b1):=StrToInt(Copy(WorkBuffer,1,PosIdx - 1));
    Delete(WorkBuffer,1,PosIdx);
   end;
  
  {Second Octet}
  PosIdx:=Pos('.',WorkBuffer);
  if PosIdx <> 0 then
   begin
    Byte(Result.S_un_b.s_b2):=StrToInt(Copy(WorkBuffer,1,PosIdx - 1));
    Delete(WorkBuffer,1,PosIdx);
   end;
  
  {Third Octet}
  PosIdx:=Pos('.',WorkBuffer);
  if PosIdx <> 0 then
   begin
    Byte(Result.S_un_b.s_b3):=StrToInt(Copy(WorkBuffer,1,PosIdx - 1));
    Delete(WorkBuffer,1,PosIdx);
   end;
   
  {Fourth Octet}
  Byte(Result.S_un_b.s_b4):=StrToInt(WorkBuffer);
 except
  {}
  LongWord(Result.S_addr):=INADDR_NONE;
 end;
end;

{==============================================================================}

function InAddrIsEqual(const AAddress1,AAddress2:TInAddr):Boolean;
{Check the supplied addresses to see if they are the same}
{Note: Expects Address to be in Host order}
begin
 {}
 Result:=(LongWord(AAddress1.S_addr) = LongWord(AAddress2.S_addr));
end;

{==============================================================================}

function InAddrIsDefault(const AAddress:TInAddr):Boolean;
{Check the supplied address to see if it is the default address}
{Note: Expects Address to be in Host order}
begin
 {}
 Result:=(LongWord(AAddress.S_addr) = INADDR_ANY);
end;

{==============================================================================}

function InAddrIsLoopback(const AAddress:TInAddr):Boolean;
{Check the supplied address to see if it is a loopback address}
{Note: Expects Address to be in Host order}
begin
 {}
 Result:=False;
 
 if LongWord(AAddress.S_addr) < LongWord(IP_LOOPBACK_NETWORK.S_addr) then Exit;
 if LongWord(AAddress.S_addr) > (LongWord(IP_LOOPBACK_NETWORK.S_addr) or not(LongWord(IP_LOOPBACK_NETMASK.S_addr))) then Exit;
 
 Result:=True;
end;

{==============================================================================}

function InAddrIsBroadcast(const AAddress:TInAddr):Boolean;
{Check the supplied address to see if it is the broadcast address}
{Note: Expects Address to be in Host order}
begin
 {}
 Result:=True;
 
 if LongWord(AAddress.S_addr) = LongWord(IP_BROADCAST_ADDRESS.S_addr) then Exit;
 
 Result:=False;
end;

{==============================================================================}

function InAddrIsMulticast(const AAddress:TInAddr):Boolean;
{Check the supplied address to see if it is a multicast address}
{Note: Expects Address to be in Host order}
begin
 {}
 Result:=False;
 
 if LongWord(AAddress.S_addr) < LongWord(IP_MULTICAST_NETWORK.S_addr) then Exit;
 if LongWord(AAddress.S_addr) > (LongWord(IP_MULTICAST_NETWORK.S_addr) or not(LongWord(IP_MULTICAST_NETMASK.S_addr))) then Exit;
 
 Result:=True;
end;

{==============================================================================}

function In6AddrToString(const AAddress:TIn6Addr):String;
begin
 {}
 Result:='';
 //To Do /
end;

{==============================================================================}

function StringToIn6Addr(const AAddress:String):TIn6Addr;
begin
 {}
 FillChar(Result,SizeOf(TIn6Addr),0);
 //To Do /
end;

{==============================================================================}

function In6AddrIsEqual(const AAddress1,AAddress2:TIn6Addr):Boolean;
{Check the supplied addresses to see if they are the same}
var
 Count:Integer;
begin
 {}
 Result:=True;
 for Count:=Low(AAddress1.u6_addr16) to High(AAddress1.u6_addr16) do
  begin
   Result:=(AAddress1.u6_addr16[Count] = AAddress2.u6_addr16[Count]) and Result;
  end; 
end;

{==============================================================================}

function In6AddrIsDefault(const AAddress:TIn6Addr):Boolean;
{Check the supplied address to see if it is the default address}
begin
 {}
 Result:=In6AddrIsEqual(AAddress,IP6_DEFAULT_ADDRESS);
end;

{==============================================================================}

function In6AddrIsLoopback(const AAddress:TIn6Addr):Boolean;
{Check the supplied address to see if it is a loopback address}
begin
 {}
 Result:=In6AddrIsEqual(AAddress,IP6_LOOPBACK_ADDRESS);
end;

{==============================================================================}

function In6AddrIsLinkLocal(const AAddress:TIn6Addr):Boolean;
{Check the supplied address to see if it is a link local address}
begin
 {}
 Result:=((AAddress.u6_addr8[0] = $FE) and ((AAddress.u6_addr8[1] and $C0) = $80));
end;

{==============================================================================}

function In6AddrIsSiteLocal(const AAddress:TIn6Addr):Boolean;
{Check the supplied address to see if it is a site local address}
begin
 {}
 Result:=((AAddress.u6_addr8[0] = $FE) and ((AAddress.u6_addr8[1] and $C0) = $C0));
end;

{==============================================================================}

function In6AddrIsV4Mapped(const AAddress:TIn6Addr):Boolean;
{Check the supplied address to see if it is a V4 mapped address}
begin
 {}
 Result:=((AAddress.u6_addr16[0] = 0) and (AAddress.u6_addr16[1] = 0) and (AAddress.u6_addr16[2] = 0) and
          (AAddress.u6_addr16[3] = 0) and (AAddress.u6_addr16[4] = 0) and (AAddress.u6_addr16[5] = $FFFF));
end;

{==============================================================================}

function In6AddrIsV4Compatible(const AAddress:TIn6Addr):Boolean;
{Check the supplied address to see if it is a V4 compatible address}
begin
 {}
 Result:=((AAddress.u6_addr16[0] = 0) and (AAddress.u6_addr16[1] = 0) and (AAddress.u6_addr16[2] = 0) and
          (AAddress.u6_addr16[3] = 0) and (AAddress.u6_addr16[4] = 0) and (AAddress.u6_addr16[5] = 0) and
      not((AAddress.u6_addr16[6] = 0) and (AAddress.s6_addr[14] = 0) and ((AAddress.s6_addr[15] = 0) or (AAddress.s6_addr[15] = 1))));
end;

{==============================================================================}

function In6AddrIsMulticast(const AAddress:TIn6Addr):Boolean;
{Check the supplied address to see if it is a multicast address}
begin
 {}
 Result:=(AAddress.u6_addr8[0] = $FF);
end;

{==============================================================================}

function In6AddrIsMulticastNodeLocal(const AAddress:TIn6Addr):Boolean;
{Check the supplied address to see if it is a multicast node local address}
begin
 {}
 Result:=In6AddrIsMulticast(AAddress) and ((AAddress.u6_addr8[1] and $F) = 1);
end;

{==============================================================================}

function In6AddrIsMulticastLinkLocal(const AAddress:TIn6Addr):Boolean;
{Check the supplied address to see if it is a multicast link local address}
begin
 {}
 Result:=In6AddrIsMulticast(AAddress) and ((AAddress.u6_addr8[1] and $F) = 2);
end;

{==============================================================================}

function In6AddrIsMulticastSiteLocal(const AAddress:TIn6Addr):Boolean;
{Check the supplied address to see if it is a multicast site local address}
begin
 {}
 Result:=In6AddrIsMulticast(AAddress) and ((AAddress.u6_addr8[1] and $F) = 5);
end;

{==============================================================================}

function In6AddrIsMulticastOrgLocal(const AAddress:TIn6Addr):Boolean;
{Check the supplied address to see if it is a multicast org local address}
begin
 {}
 Result:=In6AddrIsMulticast(AAddress) and ((AAddress.u6_addr8[1] and $F) = 8);
end;

{==============================================================================}

function In6AddrIsMulticastGlobal(const AAddress:TIn6Addr):Boolean;
{Check the supplied address to see if it is a multicast global address}
begin
 {}
 Result:=In6AddrIsMulticast(AAddress) and ((AAddress.u6_addr8[1] and $F) = $E);
end;

{==============================================================================}

function IpxAddrToString(const AAddress:TIpxAddr):String;
begin
 {}
 Result:='';
 //To Do /
end;

{==============================================================================}

function StringToIpxAddr(const AAddress:String):TIpxAddr;
begin
 {}
 FillChar(Result,SizeOf(TIpxAddr),0);
 //To Do /
end;

{==============================================================================}

function GetChecksum(ABuffer:Pointer;AOffset,ALength:Word):Word;
{Calculate the 1s Compliment Checksum of the Supplied Buffer}
var
 Data:Pointer;
 Total:LongInt;
begin
 {}
 Total:=0;
 
 {Checksum the Buffer}
 Data:=Pointer(PtrUInt(ABuffer) + AOffset);
 while ALength > 1 do
  begin
   Total:=Total + Word(Data^);
   Dec(ALength,2);
   Inc(PtrUInt(Data),2);
  end;
 if ALength > 0 then
  begin
   Total:=Total + Byte(Data^);
  end;
  
 {Return 1s Compliment}
 while (Total shr 16) > 0 do
  begin
   Total:=(Total and $FFFF) + (Total shr 16);
  end;
 Result:=not Total;
end;

{==============================================================================}

function GetChecksum2(APseudo,ABuffer:Pointer;APseudoLength,ABufferOffset,ABufferLength:Word):Word;
{Calculate the 1s Compliment Checksum of the Supplied Pseudo and Buffer}
{Note: Pseudo is expected to start from Offset 0}
var
 Data:Pointer;
 Total:LongInt;
begin
 {}
 Total:=0;
 
 {Checksum the Pseudo}
 Data:=APseudo;
 while APseudoLength > 1 do
  begin
   Total:=Total + Word(Data^);
   Dec(APseudoLength,2);
   Inc(PtrUInt(Data),2);
  end;
 if APseudoLength > 0 then
  begin
   Total:=Total + Byte(Data^);
  end;
  
 {Checksum the Buffer}
 Data:=Pointer(PtrUInt(ABuffer) + ABufferOffset);
 while ABufferLength > 1 do
  begin
   Total:=Total + Word(Data^);
   Dec(ABufferLength,2);
   Inc(PtrUInt(Data),2);
  end;
 if ABufferLength > 0 then
  begin
   Total:=Total + Byte(Data^);
  end;
  
 {Return 1s Compliment}
 while (Total shr 16) > 0 do
  begin
   Total:=(Total and $FFFF) + (Total shr 16);
  end;
 Result:=not Total;
end;

{==============================================================================}

function GetChecksum3(APseudo,AHeader,AData:Pointer;APseudoLength,AHeaderLength,ADataOffset,ADataLength:Word):Word;
{Calculate the 1s Compliment Checksum of the Supplied Pseudo, Header and Data}
{Note: Pseudo and Header are expected to start from Offset 0}
var
 Data:Pointer;
 Total:LongInt;
begin
 {}
 Total:=0;
 
 {Checksum the Pseudo}
 Data:=APseudo;
 while APseudoLength > 1 do
  begin
   Total:=Total + Word(Data^);
   Dec(APseudoLength,2);
   Inc(PtrUInt(Data),2);
  end;
 if APseudoLength > 0 then
  begin
   Total:=Total + Byte(Data^);
  end;
  
 {Checksum the Header}
 Data:=AHeader;
 while AHeaderLength > 1 do
  begin
   Total:=Total + Word(Data^);
   Dec(AHeaderLength,2);
   Inc(PtrUInt(Data),2);
  end;
 if AHeaderLength > 0 then
  begin
   Total:=Total + Byte(Data^);
  end;
  
 {Checksum the Data}
 Data:=Pointer(PtrUInt(AData) + ADataOffset);
 while ADataLength > 1 do
  begin
   Total:=Total + Word(Data^);
   Dec(ADataLength,2);
   Inc(PtrUInt(Data),2);
  end;
 if ADataLength > 0 then
  begin
   Total:=Total + Byte(Data^);
  end;
  
 {Return 1s Compliment}
 while (Total shr 16) > 0 do
  begin
   Total:=(Total and $FFFF) + (Total shr 16);
  end;
 Result:=not Total;
end;

{==============================================================================}

function GetChecksum4(APseudo,AHeader,AOptions,AData:Pointer;APseudoLength,AHeaderLength,AOptionsLength,ADataOffset,ADataLength:Word):Word;
{Calculate the 1s Compliment Checksum of the Supplied Pseudo, Header, Options and Data}
{Note: Pseudo, Header and Options are expected to start from Offset 0}
var
 Data:Pointer;
 Total:LongInt;
begin
 {}
 Total:=0;
 
 {Checksum the Pseudo}
 Data:=APseudo;
 while APseudoLength > 1 do
  begin
   Total:=Total + Word(Data^);
   Dec(APseudoLength,2);
   Inc(PtrUInt(Data),2);
  end;
 if APseudoLength > 0 then
  begin
   Total:=Total + Byte(Data^);
  end;
  
 {Checksum the Header}
 Data:=AHeader;
 while AHeaderLength > 1 do
  begin
   Total:=Total + Word(Data^);
   Dec(AHeaderLength,2);
   Inc(PtrUInt(Data),2);
  end;
 if AHeaderLength > 0 then
  begin
   Total:=Total + Byte(Data^);
  end;
  
 {Checksum the Options}
 Data:=AOptions;
 while AOptionsLength > 1 do
  begin
   Total:=Total + Word(Data^);
   Dec(AOptionsLength,2);
   Inc(PtrUInt(Data),2);
  end;
 if AOptionsLength > 0 then
  begin
   Total:=Total + Byte(Data^);
  end;
  
 {Checksum the Data}
 Data:=Pointer(PtrUInt(AData) + ADataOffset);
 while ADataLength > 1 do
  begin
   Total:=Total + Word(Data^);
   Dec(ADataLength,2);
   Inc(PtrUInt(Data),2);
  end;
 if ADataLength > 0 then
  begin
   Total:=Total + Byte(Data^);
  end;
  
 {Return 1s Compliment}
 while (Total shr 16) > 0 do
  begin
   Total:=(Total and $FFFF) + (Total shr 16);
  end;
 Result:=not Total;
end;
 
{==============================================================================}
{==============================================================================}
{Transport Helper Functions}
function HostTypeToString(AType:Word):String;
begin
 {}
 Result:='';
 
 {Check Type}
 case AType of
  HOST_TYPE_DYNAMIC:Result:='HOST_TYPE_DYNAMIC';
  HOST_TYPE_STATIC:Result:='HOST_TYPE_STATIC';
  HOST_TYPE_LOOPBACK:Result:='HOST_TYPE_LOOPBACK';
 end; 
end;

{==============================================================================}

function RouteTypeToString(AType:Word):String;
begin
 {}
 Result:='';
 
 {Check Type}
 case AType of
  ROUTE_TYPE_DYNAMIC:Result:='ROUTE_TYPE_DYNAMIC';
  ROUTE_TYPE_STATIC:Result:='ROUTE_TYPE_STATIC';
  ROUTE_TYPE_LOOPBACK:Result:='ROUTE_TYPE_LOOPBACK';
  ROUTE_TYPE_GATEWAY:Result:='ROUTE_TYPE_GATEWAY';
  ROUTE_TYPE_MULTICAST:Result:='ROUTE_TYPE_MULTICAST';
  ROUTE_TYPE_BROADCAST:Result:='ROUTE_TYPE_BROADCAST';
 end; 
end;

{==============================================================================}

function AddressTypeToString(AType:Word):String;
begin
 {}
 Result:='';
 
 {Check Type}
 case AType of
  ADDRESS_TYPE_DYNAMIC:Result:='ADDRESS_TYPE_DYNAMIC';
  ADDRESS_TYPE_STATIC:Result:='ADDRESS_TYPE_STATIC';
  ADDRESS_TYPE_LOCAL:Result:='ADDRESS_TYPE_LOCAL';
  ADDRESS_TYPE_LOOPBACK:Result:='ADDRESS_TYPE_LOOPBACK';
  ADDRESS_TYPE_MULTICAST:Result:='ADDRESS_TYPE_MULTICAST';
  ADDRESS_TYPE_BROADCAST:Result:='ADDRESS_TYPE_BROADCAST';

  ADDRESS_TYPE_PRIMARY:Result:='ADDRESS_TYPE_PRIMARY';
  ADDRESS_TYPE_SECONDARY:Result:='ADDRESS_TYPE_SECONDARY';
 end; 
end;

{==============================================================================}

function ProtocolToString(AProtocol:Word):String;
begin
 {}
 Result:='';
 
 {Check Protocol}
 case AProtocol of
  IPPROTO_IP:Result:='IPPROTO_IP';
  IPPROTO_ICMP:Result:='IPPROTO_ICMP';
  IPPROTO_IGMP:Result:='IPPROTO_IGMP';
  IPPROTO_GGP:Result:='IPPROTO_GGP';
  IPPROTO_TCP:Result:='IPPROTO_TCP';
  IPPROTO_EGP:Result:='IPPROTO_EGP';
  IPPROTO_PUP:Result:='IPPROTO_PUP';
  IPPROTO_UDP:Result:='IPPROTO_UDP';
  IPPROTO_HMP:Result:='IPPROTO_HMP';
  IPPROTO_IDP:Result:='IPPROTO_IDP';
  IPPROTO_RDP:Result:='IPPROTO_RDP';
  IPPROTO_IPV6:Result:='IPPROTO_IPV6';
  IPPROTO_ROUTING:Result:='IPPROTO_ROUTING';
  IPPROTO_FRAGMENT:Result:='IPPROTO_FRAGMENT';
  IPPROTO_ICMPV6:Result:='IPPROTO_ICMPV6';
  IPPROTO_RVD:Result:='IPPROTO_RVD';
  IPPROTO_ND:Result:='IPPROTO_ND';
  IPPROTO_RAW:Result:='IPPROTO_RAW';
 end; 
end;

{==============================================================================}

function SocketTypeToString(ASocketType:Word):String;
begin
 {}
 Result:='';
 
 {Check Socket Type}
 case ASocketType of
  SOCK_STREAM:Result:='SOCK_STREAM';
  SOCK_DGRAM:Result:='SOCK_DGRAM';
  SOCK_RAW:Result:='SOCK_RAW';
  SOCK_RDM:Result:='SOCK_RDM';
  SOCK_SEQPACKET:Result:='SOCK_SEQPACKET';
  SOCK_PACKET:Result:='SOCK_PACKET';
 end; 
end;
 
{==============================================================================}

function AddressFamilyToString(AFamily:Word):String;
begin
 {}
 Result:='';
 
 {Check Family}
 case AFamily of
  AF_UNSPEC:Result:='AF_UNSPEC';
  AF_UNIX:Result:='AF_UNIX';
  AF_INET:Result:='AF_INET';
  AF_IMPLINK:Result:='AF_IMPLINK';
  AF_PUP:Result:='AF_PUP';
  AF_CHAOS:Result:='AF_CHAOS';
  AF_IPX:Result:='AF_IPX';
  {AF_NS:Result:='AF_NS';}
  AF_ISO:Result:='AF_ISO';
  {AF_OSI:Result:='AF_OSI';}
  AF_ECMA:Result:='AF_ECMA';
  AF_DATAKIT:Result:='AF_DATAKIT';
  AF_CCITT:Result:='AF_CCITT';
  AF_SNA:Result:='AF_SNA';
  AF_DECnet:Result:='AF_DECnet';
  AF_DLI:Result:='AF_DLI';
  AF_LAT:Result:='AF_LAT';
  AF_HYLINK:Result:='AF_HYLINK';
  AF_APPLETALK:Result:='AF_APPLETALK';
  AF_NETBIOS:Result:='AF_NETBIOS';
  AF_VOICEVIEW:Result:='AF_VOICEVIEW';
  AF_FIREFOX:Result:='AF_FIREFOX';
  AF_UNKNOWN1:Result:='AF_UNKNOWN1';
  AF_BAN:Result:='AF_BAN';
  AF_ATM:Result:='AF_ATM';
  AF_INET6:Result:='AF_INET6';
  AF_CLUSTER:Result:='AF_CLUSTER';
  AF_12844:Result:='AF_12844';
  AF_IRDA:Result:='AF_IRDA';
  AF_NETDES:Result:='AF_NETDES';
 end; 
end;
 
{==============================================================================}
{==============================================================================}

initialization
 TransportInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end. 