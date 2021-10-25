{
Ultibo Network Protocol interface unit.

Copyright (C) 2021 - SoftOz Pty Ltd.

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

 
Network Protocol
================

 Notes: Includes all global protocol definitions plus base class
        for Network protocols

        Currently supported protocols are TCP, UDP, ICMP, IGMP, RAW,
        ICMPV6, IPX and SPX

 Notes: All HostToNetwork swaps occur at the level where they are
        appropriate (ie the level of their header)

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Protocol;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,Platform,Threads,Devices,SysUtils,Classes,Network,Transport,Ultibo,UltiboClasses;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
{const}
 {Protocol specific constants}
 
const
 {Generic Protocol}
 PROTOCOL_THREAD_NAME     = 'Network Protocol';     {Thread name for Network protocol threads}
 PROTOCOL_THREAD_PRIORITY = THREAD_PRIORITY_HIGHER; {Thread priority for Network protocol threads}
 
 {Protocol Timer}
 {Key Values}
 SOCKET_TIMER_KEY_NONE = TIMER_KEY_NONE;
 SOCKET_TIMER_KEY_MAX = TIMER_KEY_MAX;
 SOCKET_TIMER_KEY_MIN = TIMER_KEY_MIN;
 
 {Flag Values}
 SOCKET_TIMER_FLAG_NONE    = $00000000; 
 SOCKET_TIMER_FLAG_ACTIVE  = $00000001; {The socket timer item is active in a timer}
 SOCKET_TIMER_FLAG_DYNAMIC = $00000002; {The socket timer item was allocated dynamically}
 
 {UDP Protocol}

 {TCP Protocol}

 {ICMP Protocol}
 {Header Types}
 ICMP_ECHOREPLY     = 0;     // echo reply
 ICMP_UNREACH       = 3;     // dest unreachable
 ICMP_SOURCEQUENCH  = 4;     // packet lost, slow down
 ICMP_REDIRECT      = 5;     // shorter route
 ICMP_ECHO          = 8;     // echo service
 ICMP_ROUTERADVERT  = 9;     // router advertisement
 ICMP_ROUTERSOLICIT = 10;    // router solicitation
 ICMP_TIMXCEED      = 11;    // time exceeded
 ICMP_PARAMPROB     = 12;    // ip header bad
 ICMP_TSTAMP        = 13;    // timestamp request
 ICMP_TSTAMPREPLY   = 14;    // timestamp reply
 ICMP_IREQ          = 15;    // information request
 ICMP_IREQREPLY     = 16;    // information reply
 ICMP_MASKREQ       = 17;    // address mask request
 ICMP_MASKREPLY     = 18;    // address mask reply

 {Header Codes}
 ICMP_UNREACH_NET       = 0;               // bad net
 ICMP_UNREACH_HOST      = 1;               // bad host
 ICMP_UNREACH_PROTOCOL  = 2;               // bad protocol
 ICMP_UNREACH_PORT      = 3;               // bad port
 ICMP_UNREACH_NEEDFRAG  = 4;               // IP_DF caused drop
 ICMP_UNREACH_SRCFAIL   = 5;               // src route failed
 ICMP_UNREACH_NET_UNKNOWN = 6;             // unknown net
 ICMP_UNREACH_HOST_UNKNOWN = 7;            // unknown host
 ICMP_UNREACH_ISOLATED  = 8;               // src host isolated
 ICMP_UNREACH_NET_PROHIB = 9;              // prohibited access
 ICMP_UNREACH_HOST_PROHIB = 10;            // ditto
 ICMP_UNREACH_TOSNET    = 11;              // bad tos for net
 ICMP_UNREACH_TOSHOST   = 12;              // bad tos for host
 ICMP_UNREACH_FILTER_PROHIB = 13;          // admin prohib
 ICMP_UNREACH_HOST_PRECEDENCE = 14;        // host prec vio.
 ICMP_UNREACH_PRECEDENCE_CUTOFF = 15;      // prec cutoff

 ICMP_REDIRECT_NET      = 0;               // for network
 ICMP_REDIRECT_HOST     = 1;               // for host
 ICMP_REDIRECT_TOSNET   = 2;               // for tos and net
 ICMP_REDIRECT_TOSHOST  = 3;               // for tos and host

 ICMP_TIMXCEED_INTRANS  = 0;               // ttl=0 in transit
 ICMP_TIMXCEED_REASS    = 1;               // ttl=0 in reassembly

 ICMP_PARAMPROB_OPTABSENT = 1;             // req. opt. absent

 {IGMP Protocol}
 {Header Types}
 IGMP_VERSION  = 2;
 IGMP_QUERY    = $0011;
 IGMP_REPORTV1 = $0012;
 IGMP_REPORTV2 = $0016;
 IGMP_LEAVE    = $0017;

 {RAW Protocol}

 {ICMP6 Protocol}
 
{==============================================================================}
type
 {Protocol specific types}
 {Generic Protocol}
 PProtocolStatistics = ^TProtocolStatistics;
 TProtocolStatistics = record
  PacketsIn:Int64; 
  PacketsOut:Int64;
  BytesIn:Int64;
  BytesOut:Int64;
  ErrorsIn:Int64;
  ErrorsOut:Int64;
  PacketsLost:Int64;
 end;

 {Protocol Timer}
 PSocketTimerItem = ^TSocketTimerItem;
 TSocketTimerItem = record
  Key:Integer;                {Ordering key for timer list}
  Flags:LongWord;             {Flags for this timer item}
  Socket:TObject;             {The socket referenced by this timer list item}
  Prev:PSocketTimerItem;      {Previous item in timer list}
  Next:PSocketTimerItem;      {Next item in timer list}
 end;
 
 {UDP Protocol}

 {TCP Protocol}

 {ICMP Protocol}

 {IGMP Protocol}

 {RAW Protocol}

 {ICMP6 Protocol}
 
{==============================================================================}
type
 {Protocol specific classes}
 TNetworkProtocol = class;
 TProtocolCallback = function(AProtocol:TNetworkProtocol):Boolean of object;
 
 TNetworkFilter = class;
 TFilterCallback = function(AFilter:TNetworkFilter):Boolean of object;
 
 TNetworkConfig = class;
 TConfigCallback = function(AConfig:TNetworkConfig):Boolean of object;
 
 TProtocolSocket = class;
 TProtocolManager = class(TObject)
   constructor Create(ASettings:TNetworkSettings;ATransports:TTransportManager);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
   FSettings:TNetworkSettings;
   FTransports:TTransportManager;
   
   {Status Variables}
   FProtocols:TNetworkList;   {List of TNetworkProtocol objects}
   FFilters:TNetworkList;     {List of TNetworkFilter objects} 
   FConfigs:TNetworkList;     {List of TNetworkConfig objects}
   //FClients //To Do
   
   {Event Variables}

   {Internal Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
  public
   {Public Properties}
   property Settings:TNetworkSettings read FSettings;
   property Transports:TTransportManager read FTransports;
   
   {Public Methods}
   function AddProtocol(AProtocol:TNetworkProtocol):Boolean;
   function RemoveProtocol(AProtocol:TNetworkProtocol):Boolean;
   
   function GetProtocolByName(const AName:String;ALock:Boolean;AState:LongWord):TNetworkProtocol;
   function GetProtocolByType(AProtocol,ASocketType:Word;ALock:Boolean;AState:LongWord):TNetworkProtocol;
   function GetProtocolByProtocol(AProtocol:TNetworkProtocol;ALock:Boolean;AState:LongWord):TNetworkProtocol;
   function GetProtocolBySocket(ASocket:TProtocolSocket;ALock:Boolean;AState:LongWord):TNetworkProtocol;
   function GetProtocolByNext(APrevious:TNetworkProtocol;ALock,AUnlock:Boolean;AState:LongWord):TNetworkProtocol;
   
   function AddFilter(AFilter:TNetworkFilter):Boolean;
   function RemoveFilter(AFilter:TNetworkFilter):Boolean;
   
   function GetFilterByProtocol(AProtocol:Word;ALock:Boolean;AState:LongWord):TNetworkFilter;
   function GetFilterByFilter(AFilter:TNetworkFilter;ALock:Boolean;AState:LongWord):TNetworkFilter;
   function GetFilterByNext(APrevious:TNetworkFilter;ALock,AUnlock:Boolean;AState:LongWord):TNetworkFilter;
   
   function AddConfig(AConfig:TNetworkConfig):Boolean;
   function RemoveConfig(AConfig:TNetworkConfig):Boolean;
   
   function GetConfigByType(AConfigType:Word;ALock:Boolean;AState:LongWord):TNetworkConfig;
   function GetConfigByConfig(AConfig:TNetworkConfig;ALock:Boolean;AState:LongWord):TNetworkConfig;
   function GetConfigByNext(APrevious:TNetworkConfig;ALock,AUnlock:Boolean;AState:LongWord):TNetworkConfig;
   
   function SetConfigByType(AConfigType:Word;AInitDelay,ARetryCount,ARetryTimeout:LongWord):Boolean;
   
   //AddClient //To Do ? //TNetworkProtocol as well ? //Transport as well ?
   //RemoveClient
   
   //GetClientBy //To Do ? //TNetworkProtocol as well ? //Transport as well ?
   //GetClientBy
   //GetClientByNext
   
   function StartProtocols:Boolean;
   function StopProtocols:Boolean;
   function ProcessProtocols:Boolean;

   function ProcessSockets:Boolean;
   
   function EnumerateProtocols(ACallback:TProtocolCallback):Boolean;
   
   function BindProtocols(ATransport:TNetworkTransport):Boolean;
   function UnbindProtocols(ATransport:TNetworkTransport):Boolean;
   
   function StartFilters:Boolean;
   function StopFilters:Boolean;
   function ProcessFilters:Boolean;

   function EnumerateFilters(ACallback:TFilterCallback):Boolean;
   
   function BindFilters(ATransport:TNetworkTransport):Boolean;
   function UnbindFilters(ATransport:TNetworkTransport):Boolean;
   
   function StartConfigs:Boolean;
   function StopConfigs:Boolean;
   function ProcessConfigs:Boolean;

   function EnumerateConfigs(ACallback:TConfigCallback):Boolean;
   
   function BindConfigs(ATransport:TNetworkTransport):Boolean;
   function UnbindConfigs(ATransport:TNetworkTransport):Boolean;
   
   //StartClients  //To Do
   //StopClients
   //ProcessClients
   
   //EnumerateClients
   
   //BindClients
   //UnbindClients
   
   {Public Methods}
   function CheckSocket(ASocket:TSocket;ALock:Boolean;AState:LongWord):Boolean;

   {BSD Socket Methods}
   function Select(ANfds:Integer;AReadfds,AWritefds,AExceptfds:PFDSet;ATimeout:PTimeVal):LongInt;
   function Socket(AFamily,AStruct,AProtocol:Integer):TSocket;
 end;

 TProtocolTransport = class(TListObject) {Downstream}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   Handle:THandle;               //To Do //Do these need lock protection ?
   Protocol:Word;
   Transport:TNetworkTransport;
   
   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;

 TSocketTimer = class;
 TSocketThread = class;
 TProtocolPort = class;
 TNetworkProtocol = class(TListObject)
   constructor Create(AManager:TProtocolManager;const AName:String);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
   FLocalLock:TMutexHandle;
  protected
   {Internal Variables}
   FManager:TProtocolManager;
   FName:String;
   
   {Status Variables}
   FProtocol:Word;
   FSocketType:Word;
   FTransports:TNetworkList;   {List of TProtocolTransport objects}

   FTimer:TSocketTimer;        {Timer for socket processing}
   FThread:TSocketThread;      {Thread for socket processing}
   FPorts:TNetworkList;        {List of TProtocolPort objects}
   FSockets:TNetworkList;      {List of TProtocolSocket objects}

   FStatistics:TProtocolStatistics;

   {Event Methods}

   {Internal Methods}
   function GetName:String;
   
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
   
   {Protected Methods}
   function OpenPort(ASocket:TProtocolSocket;APort:Word):Boolean; virtual;
   function ClosePort(ASocket:TProtocolSocket):Boolean; virtual;
   function FindPort(APort:Word;AWrite,ALock:Boolean):TProtocolPort; virtual;

   function SelectGet(AReadfds,AWritefds,AExceptfds:PFDSet;var ACode:Integer):TProtocolSocket; virtual;
   
   function SelectStart(ASource,ADest:PFDSet):Boolean; virtual;
   function SelectCheck(ASource,ADest:PFDSet;ACode:Integer):Integer; virtual;
   function SelectWait(ASocket:TProtocolSocket;ACode:Integer;ATimeout:LongWord):Integer; virtual;

   function SendPacket(ASocket:TProtocolSocket;ASource,ADest:Pointer;ASourcePort,ADestPort:Word;APacket:PPacketFragment;ASize,AFlags:Integer):Integer; virtual;
  public
   {Public Properties}
   property Manager:TProtocolManager read FManager;
   property Name:String read GetName;
   
   property Protocol:Word read FProtocol;
   property SocketType:Word read FSocketType;

   {BSD Socket Methods}
   function Accept(ASocket:TProtocolSocket;ASockAddr:PSockAddr;AAddrLength:PInteger):TProtocolSocket; virtual;
   function Bind(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer; virtual;
   function CloseSocket(ASocket:TProtocolSocket):Integer; virtual;
   function Connect(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer; virtual;
   function IoctlSocket(ASocket:TProtocolSocket;ACmd:DWORD;var AArg:u_long):Integer; virtual;
   function GetPeerName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer; virtual;
   function GetSockName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer; virtual;
   function GetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer; virtual;
   function Listen(ASocket:TProtocolSocket;ABacklog:Integer):Integer; virtual;
   function Recv(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer; virtual;
   function RecvFrom(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AFromAddr:TSockAddr;var AFromLength:Integer):Integer; virtual;
   function Select(ANfds:Integer;AReadfds,AWritefds,AExceptfds:PFDSet;ATimeout:PTimeVal):LongInt; virtual;
   function Send(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer; virtual;
   function SendTo(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AToAddr:TSockAddr;AToLength:Integer):Integer; virtual;
   function SetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer; virtual;
   function Shutdown(ASocket:TProtocolSocket;AHow:Integer):Integer; virtual;
   function Socket(AFamily,AStruct,AProtocol:Integer):TProtocolSocket; virtual;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   
   function GetStatistics:TProtocolStatistics; virtual;

   function GetTransportByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TProtocolTransport;
   function GetTransportByFamily(AFamily:Word;ALock:Boolean;AState:LongWord):TProtocolTransport;
   function GetTransportByTransport(ATransport:TNetworkTransport;ALock:Boolean;AState:LongWord):TProtocolTransport;
   function GetTransportByNext(APrevious:TProtocolTransport;ALock,AUnlock:Boolean;AState:LongWord):TProtocolTransport;
   
   function AddTransport(ATransport:TNetworkTransport):Boolean; virtual;
   function RemoveTransport(ATransport:TNetworkTransport):Boolean; virtual;
   
   function GetSocketByNext(APrevious:TProtocolSocket;ALock,AUnlock:Boolean;AState:LongWord):TProtocolSocket;
   function CheckSocket(ASocket:TProtocolSocket;ALock:Boolean;AState:LongWord):Boolean; virtual;
   function FindSocket(AFamily,AStruct,AProtocol:Word;ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast,AListen,ALock:Boolean;AState:LongWord):TProtocolSocket; virtual;
   procedure FlushSockets(All:Boolean); virtual;
   
   function StartProtocol:Boolean; virtual;
   function StopProtocol:Boolean; virtual;
   function ProcessProtocol:Boolean; virtual;

   function ProcessSockets:Boolean; virtual;
   function ProcessSocket(ASocket:TProtocolSocket):Boolean; virtual;
   
   function BindProtocol(ATransport:TNetworkTransport):Boolean; virtual;
   function UnbindProtocol(ATransport:TNetworkTransport):Boolean; virtual;
   
   function CheckTimer:Boolean; virtual;
   function ProcessTimer:Boolean; virtual;
   
   function SendSocket(ASocket:TProtocolSocket):Boolean; virtual;
   
   function ScheduleSocket(ASocket:TProtocolSocket;ATimeout:LongWord):Boolean; virtual;
   function UnscheduleSocket(ASocket:TProtocolSocket):Boolean; virtual;
   
   function ScheduleSocketItem(ASocket:TProtocolSocket;AItem:PSocketTimerItem;ATimeout:LongWord):Boolean; virtual;
   function UnscheduleSocketItem(ASocket:TProtocolSocket;AItem:PSocketTimerItem):Boolean; virtual;
 end;

 TSocketTimer = class(TObject)
   constructor Create(AProtocol:TNetworkProtocol);
   destructor Destroy; override;
  protected
   {Internal Variables}
   FProtocol:TNetworkProtocol;
   
   FLock:TMutexHandle;
   FInterval:LongWord;
   FCheckTimer:TTimerHandle;
   FProcessSemaphore:TSemaphoreHandle;
   
   FCount:LongWord;
   FMaxCount:LongWord;
   
   FFirst:PSocketTimerItem;
   FLast:PSocketTimerItem;
   
   {Internal Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
   
   function Dequeue(AMax:Integer):TProtocolSocket;
   
   function FirstKey:Integer;
   function InsertKey(ASocket:TProtocolSocket;AItem:PSocketTimerItem;AKey:Integer):Boolean;
   function DeleteKey(ASocket:TProtocolSocket;AItem:PSocketTimerItem):Boolean;
   function DecrementKey:Integer;
  public   
   {Public Properties}
   property Count:LongWord read FCount;
   property MaxCount:LongWord read FMaxCount;
   
   {Public Methods}
   function StartTimer(AInterval:LongWord):Boolean; virtual;
   function StopTimer:Boolean; virtual;
   
   function CheckTimer:Boolean; virtual;
   function ProcessTimer:Boolean; virtual;
   
   function ScheduleSocket(ASocket:TProtocolSocket;ATimeout:LongWord):Boolean; virtual;
   function UnscheduleSocket(ASocket:TProtocolSocket):Boolean;  virtual;
   
   function ScheduleSocketItem(ASocket:TProtocolSocket;AItem:PSocketTimerItem;ATimeout:LongWord):Boolean; virtual;
   function UnscheduleSocketItem(ASocket:TProtocolSocket;AItem:PSocketTimerItem):Boolean; virtual;
 end;
 
 TSocketThread = class(TThread)
   constructor Create(AProtocol:TNetworkProtocol);
   destructor Destroy; override;
  protected
   {Internal Variables}
   FProtocol:TNetworkProtocol;
   
   {Internal Methods}
   procedure Execute; override;
  public   
   {Public Methods}
   function SendSocket(ASocket:TProtocolSocket):Boolean;
 end;
 
 TProtocolPort = class(TListObject)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TMutexHandle; //To Do //Should this be a CriticalSectionHandle ? //It is about the only Mutex left in Network ?
  public
   {Status Variables}
   Port:Word;     //To Do //Do these need lock protection ? //Probably should be a read only property with APort passed to create !!
   Sockets:TList; //To Do //Should this be something better than a TList ?
   
   {Public Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
 end;

 TProtocolState = class;
 TProtocolOptions = class;
 TProtocolSocket = class(TTransportSocket) 
   constructor Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
   destructor Destroy; override;
  private
   {Internal Variables}
   FSocketChange:TEventHandle;
  protected
   {Internal Variables}
   FProtocol:TNetworkProtocol;

   FScheduled:Boolean;
   
   {Protocol Layer Variables}
   FProtocolState:TProtocolState;
   FProtocolOptions:TProtocolOptions;
   
   {Internal Methods}
   function CheckSocket(ASocket:TProtocolSocket;ALock:Boolean;AState:LongWord):Boolean; 
  public
   {Protocol Layer Properties}
   property ProtocolState:TProtocolState read FProtocolState;
   property ProtocolOptions:TProtocolOptions read FProtocolOptions;

   {Public Properties}
   property Protocol:TNetworkProtocol read FProtocol;

   property Scheduled:Boolean read FScheduled write FScheduled;
   
   {Public Methods}
   function WaitChange:Boolean;
   function WaitChangeEx(ATimeout:LongWord):Boolean;
   function SignalChange:Boolean;
   
   function SendSocket:Boolean; virtual;
   
   function ScheduleSocket(ATimeout:LongWord):Boolean; virtual;
   function UnscheduleSocket:Boolean;  virtual;
   
   function ScheduleSocketItem(AItem:PSocketTimerItem;ATimeout:LongWord):Boolean; virtual;
   function UnscheduleSocketItem(AItem:PSocketTimerItem):Boolean; virtual;
 end;

 TProtocolState = class(TObject)
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TCriticalSectionHandle;
  protected
   {Internal Variables}
   FLocalPort:Word;
   FRemotePort:Word;
   
   {Internal Methods}
   function AcquireLock:Boolean;
   function ReleaseLock:Boolean;
   
   procedure SetLocalPort(ALocalPort:Word);
   procedure SetRemotePort(ARemotePort:Word);
  public
   {Status Variables}
   property LocalPort:Word read FLocalPort write SetLocalPort;
   property RemotePort:Word read FRemotePort write SetRemotePort;
 end;

 TProtocolOptions = class(TObject) {For Get/Set Options at the Protocol Level (eg IPPROTO_TCP, IPPROTO_UDP)}
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

 TFilterTransport = class(TListObject) {Downstream}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   Handle:THandle;                //To Do //Do these need lock protection ?
   Protocol:Word;
   Transport:TNetworkTransport;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;

 TNetworkFilter = class(TListObject) {eg IP Filter}
   constructor Create(AManager:TProtocolManager);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  protected
   {Internal Variables}
   FManager:TProtocolManager;

   {Status Variables}
   FProtocol:Word;
   FTransports:TNetworkList;   {List of TFilterTransport objects}

   {Event Methods}

   {Internal Methods}
   
  public
   {Public Properties}
   property Manager:TProtocolManager read FManager;
   property Protocol:Word read FProtocol;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   
   function GetTransportByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TFilterTransport;
   function GetTransportByFamily(AFamily:Word;ALock:Boolean;AState:LongWord):TFilterTransport;
   function GetTransportByTransport(ATransport:TNetworkTransport;ALock:Boolean;AState:LongWord):TFilterTransport;
   function GetTransportByNext(APrevious:TFilterTransport;ALock,AUnlock:Boolean;AState:LongWord):TFilterTransport;

   function AddTransport(ATransport:TNetworkTransport):Boolean; virtual;
   function RemoveTransport(ATransport:TNetworkTransport):Boolean; virtual;
   
   function StartFilter:Boolean; virtual;
   function StopFilter:Boolean; virtual;
   function ProcessFilter:Boolean; virtual;

   function BindFilter(ATransport:TNetworkTransport):Boolean; virtual;
   function UnbindFilter(ATransport:TNetworkTransport):Boolean; virtual;
 end;

 TConfigTransport = class(TListObject) {Downstream}
   constructor Create;
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  public
   {Status Variables}
   Handle:THandle;                //To Do //Do these need lock protection ?
   ConfigType:Word;
   Transport:TNetworkTransport;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
 end;

 TNetworkConfig = class(TListObject) {eg DHCP/BOOTP/RARP/STATIC/PSEUDO}
   constructor Create(AManager:TProtocolManager);
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  protected
   {Internal Variables}
   FManager:TProtocolManager;

   {Status Variables}
   FConfigType:Word;
   FInitDelay:LongWord;
   FRetryCount:LongWord;
   FRetryTimeout:LongWord;
   FTransports:TNetworkList;   {List of TConfigTransport objects}

   {Event Methods}

   {Internal Methods}
   
  public
   {Public Properties}
   property Manager:TProtocolManager read FManager;
   property ConfigType:Word read FConfigType;
   property InitDelay:LongWord read FInitDelay;
   property RetryCount:LongWord read FRetryCount;
   property RetryTimeout:LongWord read FRetryTimeout;

   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   
   function GetTransportByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TConfigTransport;
   function GetTransportByFamily(AFamily:Word;ALock:Boolean;AState:LongWord):TConfigTransport;
   function GetTransportByTransport(ATransport:TNetworkTransport;ALock:Boolean;AState:LongWord):TConfigTransport;
   function GetTransportByNext(APrevious:TConfigTransport;ALock,AUnlock:Boolean;AState:LongWord):TConfigTransport;

   function AddTransport(ATransport:TNetworkTransport):Boolean; virtual;
   function RemoveTransport(ATransport:TNetworkTransport):Boolean; virtual;
   
   function StartConfig:Boolean; virtual;
   function StopConfig:Boolean; virtual;
   function ProcessConfig:Boolean; virtual;

   function BindConfig(ATransport:TNetworkTransport):Boolean; virtual;
   function UnbindConfig(ATransport:TNetworkTransport):Boolean; virtual;
   
   function SetConfig(AInitDelay,ARetryCount,ARetryTimeout:LongWord):Boolean; virtual;
 end;

 //To Do //TClientProtocol = class(TListObject) {Downstream}
 
 TNetworkClient = class(TListObject) {eg DNS/WINS}
   constructor Create(AProtocol:TNetworkProtocol); //To Do //AManager instead ?
   destructor Destroy; override;
  private
   {Internal Variables}
   FLock:TSynchronizerHandle;
  protected
   {Internal Variables}
   FProtocol:TNetworkProtocol;

   {Status Variables}
   //FThread //To Do ?
   
   {Event Methods}

   {Internal Methods}
   //GetProtocolBy //To Do ?
   //GetProtocolBy //To Do ?
   //GetProtocolBy //To Do ?
   
   {Protected Methods}
   //AddProtocol //To Do ?
   //RemoveProtocol //To Do ?
  public
   {Public Properties}
   property Protocol:TNetworkProtocol read FProtocol;
   
   {Public Methods}
   function ReaderLock:Boolean;
   function ReaderUnlock:Boolean;
   function WriterLock:Boolean;
   function WriterUnlock:Boolean;
   
   function StartClient:Boolean; virtual;
   function StopClient:Boolean; virtual;
   
   //BindClient(AProtocol //To Do ?
   //UnbindClient //To Do ?
 end;
 
 //To Do //TClientThread  (For DNS etc)
 
{==============================================================================}
var
 {Protocol specific variables}
 ProtocolManager:TProtocolManager;
 
{==============================================================================}
{Initialization Functions}
procedure ProtocolInit;
function ProtocolStart:LongWord;
function ProtocolStop:LongWord;

{==============================================================================}
{Protocol Functions}
 
{==============================================================================}
{Protocol Helper Functions}
procedure ProtocolCheckTimer(Data:Pointer);

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Protocol specific variables}
 ProtocolInitialized:Boolean;
 ProtocolStarted:Boolean;

{==============================================================================}
{==============================================================================}
{TProtocolManager}
constructor TProtocolManager.Create(ASettings:TNetworkSettings;ATransports:TTransportManager);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 FSettings:=ASettings;
 FTransports:=ATransports;
 
 FProtocols:=TNetworkList.Create;
 FFilters:=TNetworkList.Create;
 FConfigs:=TNetworkList.Create;
end;

{==============================================================================}

destructor TProtocolManager.Destroy;
begin
 {}
 WriterLock;
 try
  FConfigs.Free;
  FFilters.Free;
  FProtocols.Free;
  FSettings:=nil;
  FTransports:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TProtocolManager.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtocolManager.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtocolManager.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtocolManager.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtocolManager.AddProtocol(AProtocol:TNetworkProtocol):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: AddProtocol');
  {$ENDIF}

  {Acquire Lock}
  FProtocols.WriterLock;
  try
   {Add Protocol}
   Result:=FProtocols.Add(AProtocol);
  finally 
   {Release Lock}
   FProtocols.WriterUnlock;
  end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.RemoveProtocol(AProtocol:TNetworkProtocol):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: RemoveProtocol');
  {$ENDIF}

  {Acquire Lock}
  FProtocols.WriterLock;
  try
   {Remove Protocol}
   Result:=FProtocols.Remove(AProtocol);
  finally 
   {Release Lock}
   FProtocols.WriterUnlock;
  end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.GetProtocolByName(const AName:String;ALock:Boolean;AState:LongWord):TNetworkProtocol;
var
 Protocol:TNetworkProtocol;
begin
 {}
 FProtocols.ReaderLock;
 try
  Result:=nil;
  
  {Check Name}
  if Length(AName) = 0 then Exit;
  
  {Get Protocol}
  Protocol:=TNetworkProtocol(FProtocols.First);
  while Protocol <> nil do
   begin
    {Check Protocol}
    if Uppercase(Protocol.Name) = Uppercase(AName) then
     begin
      {Lock Protocol} 
      if ALock then if AState = NETWORK_LOCK_READ then Protocol.ReaderLock else Protocol.WriterLock;
      
      {Return Result}
      Result:=Protocol;
      Exit;
     end;
     
    {Get Next}
    Protocol:=TNetworkProtocol(Protocol.Next);
   end;
 finally 
  FProtocols.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.GetProtocolByType(AProtocol,ASocketType:Word;ALock:Boolean;AState:LongWord):TNetworkProtocol;
var
 Protocol:TNetworkProtocol;
begin
 {}
 FProtocols.ReaderLock;
 try
  Result:=nil;
  
  {Get Protocol}
  Protocol:=TNetworkProtocol(FProtocols.First);
  while Protocol <> nil do
   begin
    {Check Protocol}
    if (Protocol.Protocol = AProtocol) and (Protocol.SocketType = ASocketType) then
     begin
      {Lock Protocol} 
      if ALock then if AState = NETWORK_LOCK_READ then Protocol.ReaderLock else Protocol.WriterLock;
      
      {Return Result}
      Result:=Protocol;
      Exit;
     end;
     
    {Get Next}
    Protocol:=TNetworkProtocol(Protocol.Next);
   end;
 finally 
  FProtocols.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.GetProtocolByProtocol(AProtocol:TNetworkProtocol;ALock:Boolean;AState:LongWord):TNetworkProtocol;
var
 Protocol:TNetworkProtocol;
begin
 {}
 FProtocols.ReaderLock;
 try
  Result:=nil;
  
  {Get Protocol}
  Protocol:=TNetworkProtocol(FProtocols.First);
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
    Protocol:=TNetworkProtocol(Protocol.Next);
   end;
 finally 
  FProtocols.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.GetProtocolBySocket(ASocket:TProtocolSocket;ALock:Boolean;AState:LongWord):TNetworkProtocol;
var
 Protocol:TNetworkProtocol;
begin
 Result:=nil;
 
 {Check Socket}
 if not CheckSocket(TSocket(ASocket),True,NETWORK_LOCK_READ) then Exit;
 try
  FProtocols.ReaderLock;
  try
   Result:=nil;
   
   {Get Protocol}
   Protocol:=TNetworkProtocol(FProtocols.First);
   while Protocol <> nil do
    begin
     {Check Protocol}
     if Protocol = ASocket.Protocol then
      begin
       {Lock Protocol} 
       if ALock then if AState = NETWORK_LOCK_READ then Protocol.ReaderLock else Protocol.WriterLock;
       
       {Return Result}
       Result:=Protocol;
       Exit;
      end;
      
     {Get Next} 
     Protocol:=TNetworkProtocol(Protocol.Next);
    end;
  finally 
   FProtocols.ReaderUnlock;
  end; 
 finally
  {Unlock Socket}
  ASocket.ReaderUnlock;
 end;
end;

{==============================================================================}

function TProtocolManager.GetProtocolByNext(APrevious:TNetworkProtocol;ALock,AUnlock:Boolean;AState:LongWord):TNetworkProtocol;
var
 Protocol:TNetworkProtocol;
begin
 {}
 FProtocols.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Protocol:=TNetworkProtocol(FProtocols.First);
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
    Protocol:=TNetworkProtocol(APrevious.Next);
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

function TProtocolManager.AddFilter(AFilter:TNetworkFilter):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: AddFilter');
  {$ENDIF}

  {Acquire Lock}
  FFilters.WriterLock;
  try
   {Add Filter}
   Result:=FFilters.Add(AFilter);
  finally 
   {Release Lock}
   FFilters.WriterUnlock;
  end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.RemoveFilter(AFilter:TNetworkFilter):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: RemoveFilter');
  {$ENDIF}

  {Acquire Lock}
  FFilters.WriterLock;
  try
   {Remove Filter}
   Result:=FFilters.Remove(AFilter);
  finally 
   {Release Lock}
   FFilters.WriterUnlock;
  end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.GetFilterByProtocol(AProtocol:Word;ALock:Boolean;AState:LongWord):TNetworkFilter;
var
 Filter:TNetworkFilter;
begin
 {}
 FFilters.ReaderLock;
 try
  Result:=nil;
  
  {Get Filter}
  Filter:=TNetworkFilter(FFilters.First);
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
    Filter:=TNetworkFilter(Filter.Next);
   end;
 finally 
  FFilters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.GetFilterByFilter(AFilter:TNetworkFilter;ALock:Boolean;AState:LongWord):TNetworkFilter;
var
 Filter:TNetworkFilter;
begin
 {}
 FFilters.ReaderLock;
 try
  Result:=nil;
  
  {Get Filter}
  Filter:=TNetworkFilter(FFilters.First);
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
    Filter:=TNetworkFilter(Filter.Next);
   end;
 finally 
  FFilters.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.GetFilterByNext(APrevious:TNetworkFilter;ALock,AUnlock:Boolean;AState:LongWord):TNetworkFilter;
var
 Filter:TNetworkFilter;
begin
 {}
 FFilters.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Filter:=TNetworkFilter(FFilters.First);
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
    Filter:=TNetworkFilter(APrevious.Next);
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

function TProtocolManager.AddConfig(AConfig:TNetworkConfig):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: AddConfig');
  {$ENDIF}

  {Acquire Lock}
  FConfigs.WriterLock;
  try
   {Add Config}
   Result:=FConfigs.Add(AConfig);
  finally 
   {Release Lock}
   FConfigs.WriterUnlock;
  end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.RemoveConfig(AConfig:TNetworkConfig):Boolean;
begin
 {}
 ReaderLock;
 try
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: RemoveConfig');
  {$ENDIF}

  {Acquire Lock}
  FConfigs.WriterLock;
  try
   {Remove Config}
   Result:=FConfigs.Remove(AConfig);
  finally 
   {Release Lock}
   FConfigs.WriterUnlock;
  end; 
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.GetConfigByType(AConfigType:Word;ALock:Boolean;AState:LongWord):TNetworkConfig;
var
 Config:TNetworkConfig;
begin
 {}
 FConfigs.ReaderLock;
 try
  Result:=nil;
  
  {Get Config}
  Config:=TNetworkConfig(FConfigs.First);
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
    Config:=TNetworkConfig(Config.Next);
   end;
 finally 
  FConfigs.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.GetConfigByConfig(AConfig:TNetworkConfig;ALock:Boolean;AState:LongWord):TNetworkConfig;
var
 Config:TNetworkConfig;
begin
 {}
 FConfigs.ReaderLock;
 try
  Result:=nil;
  
  {Get Config}
  Config:=TNetworkConfig(FConfigs.First);
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
    Config:=TNetworkConfig(Config.Next);
   end;
 finally 
  FConfigs.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.GetConfigByNext(APrevious:TNetworkConfig;ALock,AUnlock:Boolean;AState:LongWord):TNetworkConfig;
var
 Config:TNetworkConfig;
begin
 {}
 FConfigs.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Config:=TNetworkConfig(FConfigs.First);
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
    Config:=TNetworkConfig(APrevious.Next);
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

function TProtocolManager.SetConfigByType(AConfigType:Word;AInitDelay,ARetryCount,ARetryTimeout:LongWord):Boolean;
var
 Config:TNetworkConfig;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {Get Config}
  Config:=GetConfigByType(AConfigType,True,NETWORK_LOCK_WRITE); 
  if Config <> nil then
   begin
    {Set Config}
    Result:=Config.SetConfig(AInitDelay,ARetryCount,ARetryTimeout);
   
    {Unlock Config}
    Config.WriterUnlock;
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.StartProtocols:Boolean;
var
 Protocol:TNetworkProtocol;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: StartProtocols');
  {$ENDIF}
  
  {Get Protocol}
  Protocol:=TNetworkProtocol(GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ));
  while Protocol <> nil do
   begin
    {Start Protocol}
    if not(Protocol.StartProtocol) then Result:=False;
    
    {Get Next}
    Protocol:=TNetworkProtocol(GetProtocolByNext(Protocol,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.StopProtocols:Boolean;
var
 Protocol:TNetworkProtocol;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: StopProtocols');
  {$ENDIF}
  
  {Get Protocol}
  Protocol:=TNetworkProtocol(GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ));
  while Protocol <> nil do
   begin
    {Stop Protocol}
    if not(Protocol.StopProtocol) then Result:=False;
    
    {Get Next}
    Protocol:=TNetworkProtocol(GetProtocolByNext(Protocol,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.ProcessProtocols:Boolean;
var
 Protocol:TNetworkProtocol;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: ProcessProtocols');
  {$ENDIF}
  
  {Get Protocol}
  Protocol:=TNetworkProtocol(GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ));
  while Protocol <> nil do
   begin
    {Process Protocol}
    if not(Protocol.ProcessProtocol) then Result:=False;
    
    {Get Next}
    Protocol:=TNetworkProtocol(GetProtocolByNext(Protocol,True,False,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.ProcessSockets:Boolean;
var
 Protocol:TNetworkProtocol;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: ProcessSockets');
  {$ENDIF}
  
  {Get Protocol}
  Protocol:=TNetworkProtocol(GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ));
  while Protocol <> nil do
   begin
    {Process Sockets}
    if not(Protocol.ProcessSockets) then Result:=False;
    
    {Get Next}
    Protocol:=TNetworkProtocol(GetProtocolByNext(Protocol,True,False,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.EnumerateProtocols(ACallback:TProtocolCallback):Boolean;
var
 Protocol:TNetworkProtocol;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {Check Callback}
  if not Assigned(ACallback) then Exit;

  Result:=True;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: EnumerateProtocols');
  {$ENDIF}
  
  {Get Protocol}
  Protocol:=TNetworkProtocol(GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ));
  while Protocol <> nil do
   begin
    {Enumerate Protocol}
    if not(ACallback(Protocol)) then Result:=False;
    
    {Get Next}
    Protocol:=TNetworkProtocol(GetProtocolByNext(Protocol,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.BindProtocols(ATransport:TNetworkTransport):Boolean;
var
 Protocol:TNetworkProtocol;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: BindProtocols');
  {$ENDIF}

  {Check Transport}
  if ATransport = nil then
   begin
    {Check Transports}
    if FTransports = nil then Exit;
    
    {Enumerate Transports}
    Result:=FTransports.EnumerateTransports(BindProtocols);
   end
  else
   begin
    Result:=True;
  
    {Get Protocol}
    Protocol:=TNetworkProtocol(GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ));
    while Protocol <> nil do
     begin
      {Bind Protocol}
      if not(Protocol.BindProtocol(ATransport)) then Result:=False;
    
      {Get Next}
      Protocol:=TNetworkProtocol(GetProtocolByNext(Protocol,True,True,NETWORK_LOCK_READ));
     end;
   end;  
 finally 
  ReaderUnlock;
 end; 
end;
  
{==============================================================================}

function TProtocolManager.UnbindProtocols(ATransport:TNetworkTransport):Boolean;
var
 Protocol:TNetworkProtocol;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: UnbindProtocols');
  {$ENDIF}

  {Check Transport}
  if ATransport = nil then
   begin
    {Check Transports}
    if FTransports = nil then Exit;
    
    {Enumerate Transports}
    Result:=FTransports.EnumerateTransports(UnbindProtocols);
   end
  else
   begin
    Result:=True;
  
    {Get Protocol}
    Protocol:=TNetworkProtocol(GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ));
    while Protocol <> nil do
     begin
      {Unbind Protocol}
      if not(Protocol.UnbindProtocol(ATransport)) then Result:=False;
    
      {Get Next}
      Protocol:=TNetworkProtocol(GetProtocolByNext(Protocol,True,True,NETWORK_LOCK_READ));
     end;
   end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.StartFilters:Boolean;
var
 Filter:TNetworkFilter;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: StartFilters');
  {$ENDIF}
  
  {Get Filter}
  Filter:=TNetworkFilter(GetFilterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Filter <> nil do
   begin
    {Start Filter}
    if not(Filter.StartFilter) then Result:=False;
    
    {Get Next}
    Filter:=TNetworkFilter(GetFilterByNext(Filter,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.StopFilters:Boolean;
var
 Filter:TNetworkFilter;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: StopFilters');
  {$ENDIF}
  
  {Get Filter}
  Filter:=TNetworkFilter(GetFilterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Filter <> nil do
   begin
    {Stop Filter}
    if not(Filter.StopFilter) then Result:=False;
    
    {Get Next}
    Filter:=TNetworkFilter(GetFilterByNext(Filter,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.ProcessFilters:Boolean;
var
 Filter:TNetworkFilter;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: ProcessFilters');
  {$ENDIF}
  
  {Get Filter}
  Filter:=TNetworkFilter(GetFilterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Filter <> nil do
   begin
    {Process Filter}
    if not(Filter.ProcessFilter) then Result:=False;
    
    {Get Next}
    Filter:=TNetworkFilter(GetFilterByNext(Filter,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.EnumerateFilters(ACallback:TFilterCallback):Boolean;
var
 Filter:TNetworkFilter;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {Check Callback}
  if not Assigned(ACallback) then Exit;

  Result:=True;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: EnumerateFilters');
  {$ENDIF}
  
  {Get Filter}
  Filter:=TNetworkFilter(GetFilterByNext(nil,True,False,NETWORK_LOCK_READ));
  while Filter <> nil do
   begin
    {Enumerate Filter}
    if not(ACallback(Filter)) then Result:=False;
    
    {Get Next}
    Filter:=TNetworkFilter(GetFilterByNext(Filter,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.BindFilters(ATransport:TNetworkTransport):Boolean;
var
 Filter:TNetworkFilter;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: BindFilters');
  {$ENDIF}

  {Check Transport}
  if ATransport = nil then
   begin
    {Check Transports}
    if FTransports = nil then Exit;
    
    {Enumerate Transports}
    Result:=FTransports.EnumerateTransports(BindFilters);
   end
  else
   begin
    Result:=True;
  
    {Get Filter}
    Filter:=TNetworkFilter(GetFilterByNext(nil,True,False,NETWORK_LOCK_READ));
    while Filter <> nil do
     begin
      {Bind Filter}
      if not(Filter.BindFilter(ATransport)) then Result:=False;
    
      {Get Next}
      Filter:=TNetworkFilter(GetFilterByNext(Filter,True,True,NETWORK_LOCK_READ));
     end;
   end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.UnbindFilters(ATransport:TNetworkTransport):Boolean;
var
 Filter:TNetworkFilter;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: UnbindFilters');
  {$ENDIF}

  {Check Transport}
  if ATransport = nil then
   begin
    {Check Transports}
    if FTransports = nil then Exit;
    
    {Enumerate Transports}
    Result:=FTransports.EnumerateTransports(UnbindFilters);
   end
  else
   begin
    Result:=True;
  
    {Get Filter}
    Filter:=TNetworkFilter(GetFilterByNext(nil,True,False,NETWORK_LOCK_READ));
    while Filter <> nil do
     begin
      {Unbind Filter}
      if not(Filter.UnbindFilter(ATransport)) then Result:=False;
    
      {Get Next}
      Filter:=TNetworkFilter(GetFilterByNext(Filter,True,True,NETWORK_LOCK_READ));
     end;
   end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.StartConfigs:Boolean;
var
 Config:TNetworkConfig;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: StartConfigs');
  {$ENDIF}
  
  {Get Config}
  Config:=TNetworkConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
  while Config <> nil do
   begin
    {Start Config}
    if not(Config.StartConfig) then Result:=False;
    
    {Get Next}
    Config:=TNetworkConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.StopConfigs:Boolean;
var
 Config:TNetworkConfig;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: StopConfigs');
  {$ENDIF}
  
  {Get Config}
  Config:=TNetworkConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
  while Config <> nil do
   begin
    {Stop Config}
    if not(Config.StopConfig) then Result:=False;
    
    {Get Next}
    Config:=TNetworkConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.ProcessConfigs:Boolean;
var
 Config:TNetworkConfig;
begin
 {}
 ReaderLock;
 try
  Result:=True;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: ProcessConfigs');
  {$ENDIF}
  
  {Get Config}
  Config:=TNetworkConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
  while Config <> nil do
   begin
    {Process Config}
    if not(Config.ProcessConfig) then Result:=False;
    
    {Get Next}
    Config:=TNetworkConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.EnumerateConfigs(ACallback:TConfigCallback):Boolean;
var
 Config:TNetworkConfig;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {Check Callback}
  if not Assigned(ACallback) then Exit;

  Result:=True;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: EnumerateConfigs');
  {$ENDIF}
  
  {Get Config}
  Config:=TNetworkConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
  while Config <> nil do
   begin
    {Enumerate Config}
    if not(ACallback(Config)) then Result:=False;
    
    {Get Next}
    Config:=TNetworkConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.BindConfigs(ATransport:TNetworkTransport):Boolean;
var
 Config:TNetworkConfig;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: BindConfigs');
  {$ENDIF}

  {Check Transport}
  if ATransport = nil then
   begin
    {Check Transports}
    if FTransports = nil then Exit;
    
    {Enumerate Transports}
    Result:=FTransports.EnumerateTransports(BindConfigs);
   end
  else
   begin
    Result:=True;
  
    {Get Config}
    Config:=TNetworkConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
    while Config <> nil do
     begin
      {Bind Config}
      if not(Config.BindConfig(ATransport)) then Result:=False;
    
      {Get Next}
      Config:=TNetworkConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
     end;
   end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.UnbindConfigs(ATransport:TNetworkTransport):Boolean;
var
 Config:TNetworkConfig;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'ProtocolManager: UnbindConfigs');
  {$ENDIF}

  {Check Transport}
  if ATransport = nil then
   begin
    {Check Transports}
    if FTransports = nil then Exit;
    
    {Enumerate Transports}
    Result:=FTransports.EnumerateTransports(UnbindConfigs);
   end
  else
   begin
    Result:=True;
  
    {Get Config}
    Config:=TNetworkConfig(GetConfigByNext(nil,True,False,NETWORK_LOCK_READ));
    while Config <> nil do
     begin
      {Unbind Config}
      if not(Config.UnbindConfig(ATransport)) then Result:=False;
    
      {Get Next}
      Config:=TNetworkConfig(GetConfigByNext(Config,True,True,NETWORK_LOCK_READ));
     end;
   end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.CheckSocket(ASocket:TSocket;ALock:Boolean;AState:LongWord):Boolean;
var
 Protocol:TNetworkProtocol;
begin
 {}
 ReaderLock;
 try
  Result:=False;
  
  {Get Protocol}
  Protocol:=TNetworkProtocol(GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ));
  while Protocol <> nil do
   begin
    {Check Socket}
    Result:=Protocol.CheckSocket(TProtocolSocket(ASocket),ALock,AState);
    if Result then
     begin
      {Unlock Protocol}
      Protocol.ReaderUnlock;
      Exit;
     end;
    
    {Get Next}
    Protocol:=TNetworkProtocol(GetProtocolByNext(Protocol,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.Select(ANfds:Integer;AReadfds,AWritefds,AExceptfds:PFDSet;ATimeout:PTimeVal):LongInt;
var
 Socket:TProtocolSocket;
 Protocol:TNetworkProtocol;
begin
 {}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAENOTSOCK);
 
 if not ReaderLock then Exit;
 try
  {Setup Defaults}
  Socket:=nil;
  Protocol:=nil;
  
  {Check Sets}
  if (AReadfds <> nil) and (AReadfds.fd_count > 0) then
   begin
    {Get Socket}
    Socket:=TProtocolSocket(AReadfds.fd_array[0]);
   end
  else if (AWritefds <> nil) and (AWritefds.fd_count > 0) then
   begin
    {Get Socket}
    Socket:=TProtocolSocket(AWritefds.fd_array[0]);
   end
  else if (AExceptfds <> nil) and (AExceptfds.fd_count > 0) then
   begin
    {Get Socket}
    Socket:=TProtocolSocket(AExceptfds.fd_array[0]);
   end;
    
  {Check Socket}  
  if Socket = nil then Exit;

  {Get Protocol}
  Protocol:=GetProtocolBySocket(Socket,True,NETWORK_LOCK_READ);
  if Protocol = nil then Exit;

  {Select Socket}
  Result:=Protocol.Select(ANfds,AReadfds,AWritefds,AExceptfds,ATimeout);
      
  {Unlock Protocol}
  Protocol.ReaderUnlock;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TProtocolManager.Socket(AFamily,AStruct,AProtocol:Integer):TSocket;
var
 Protocol:TNetworkProtocol;
begin
 {}
 Result:=INVALID_SOCKET;
 NetworkSetLastError(WSAEPROTONOSUPPORT);
 
 if not ReaderLock then Exit;
 try
  {Get Protocol}  
  Protocol:=TNetworkProtocol(GetProtocolByNext(nil,True,False,NETWORK_LOCK_READ));
  while Protocol <> nil do
   begin
    {Check Protocol} {Note: IPPROTO_IP matches any Protocol}
    if (Protocol.Protocol = AProtocol) or (AProtocol = IPPROTO_IP) then
     begin
      {Create Socket}
      Result:=TSocket(Protocol.Socket(AFamily,AStruct,AProtocol));
      if Result <> INVALID_SOCKET then
       begin
        {Unlock Protocol}
        Protocol.ReaderUnlock;
        Exit;
       end;
     end;
     
    {Get Next} 
    Protocol:=TNetworkProtocol(GetProtocolByNext(Protocol,True,True,NETWORK_LOCK_READ));
   end;
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{TProtocolTransport}
constructor TProtocolTransport.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 Handle:=INVALID_HANDLE_VALUE;
 Protocol:=IPPROTO_IP;
 Transport:=nil;
end;

{==============================================================================}

destructor TProtocolTransport.Destroy;
begin
 {}
 WriterLock;
 try
  Transport:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TProtocolTransport.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtocolTransport.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtocolTransport.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtocolTransport.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TNetworkProtocol}
constructor TNetworkProtocol.Create(AManager:TProtocolManager;const AName:String);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 FLocalLock:=MutexCreate;
 
 FManager:=AManager;
 FName:=AName;
 
 FProtocol:=IPPROTO_IP;
 FSocketType:=SOCK_RAW;
 FTransports:=TNetworkList.Create;
 FTimer:=nil;
 FThread:=nil;
 FPorts:=TNetworkList.Create;
 FSockets:=TNetworkList.Create;
 FillChar(FStatistics,SizeOf(TProtocolStatistics),0);
 if FManager <> nil then FManager.AddProtocol(Self);
end;

{==============================================================================}

destructor TNetworkProtocol.Destroy;
begin
 {}
 WriterLock;
 try
  if FManager <> nil then FManager.RemoveProtocol(Self);
  FSockets.Free;
  FPorts.Free;
  FThread:=nil;
  FTimer:=nil;
  FTransports.Free;
  FProtocol:=IPPROTO_IP;
  FManager:=nil;
  MutexDestroy(FLocalLock);
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TNetworkProtocol.GetName:String;
begin
 {}
 Result:='';
   
 if not AcquireLock then Exit;
   
 Result:=FName;
 UniqueString(Result);
   
 ReleaseLock;
end;
   
{==============================================================================}

function TNetworkProtocol.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkProtocol.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLocalLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkProtocol.OpenPort(ASocket:TProtocolSocket;APort:Word):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkProtocol.ClosePort(ASocket:TProtocolSocket):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkProtocol.FindPort(APort:Word;AWrite,ALock:Boolean):TProtocolPort;
begin
 {Virtual Base Method}
 Result:=nil;
end;

{==============================================================================}

function TNetworkProtocol.SelectGet(AReadfds,AWritefds,AExceptfds:PFDSet;var ACode:Integer):TProtocolSocket;
{Determine if select was called with a single socket to be checked}
{Readfds, Writefds and Exceptfds are the working sets to check}
begin
 {}
 Result:=nil;
 
 {Setup Defaults}
 ACode:=SELECT_UNKNOWN;
 
 {Check Sets}
 if (AReadfds <> nil) and (AReadfds.fd_count = 1) then
  begin
   {Check Write}
   if (AWritefds <> nil) and (AWritefds.fd_count > 0) then Exit;
   
   {Check Except}
   if (AExceptfds <> nil) and (AExceptfds.fd_count > 0) then Exit;
   
   {Get Socket}
   Result:=TProtocolSocket(AReadfds.fd_array[0]);
   if Result = nil then Exit;
   
   {Return Code}
   ACode:=SELECT_READ;
  end
 else if (AWritefds <> nil) and (AWritefds.fd_count = 1) then
  begin
   {Check Read}
   if (AReadfds <> nil) and (AReadfds.fd_count > 0) then Exit;
   
   {Check Except}
   if (AExceptfds <> nil) and (AExceptfds.fd_count > 0) then Exit;
   
   {Get Socket}
   Result:=TProtocolSocket(AWritefds.fd_array[0]);
   if Result = nil then Exit;
   
   {Return Code}
   ACode:=SELECT_WRITE;
  end
 else if (AExceptfds <> nil) and (AExceptfds.fd_count = 1) then
  begin
   {Check Read}
   if (AReadfds <> nil) and (AReadfds.fd_count > 0) then Exit;
   
   {Check Write}
   if (AWritefds <> nil) and (AWritefds.fd_count > 0) then Exit;
   
   {Get Socket}
   Result:=TProtocolSocket(AExceptfds.fd_array[0]);
   if Result = nil then Exit;
   
   {Return Code}
   ACode:=SELECT_ERROR;
  end;
end;

{==============================================================================}

function TNetworkProtocol.SelectStart(ASource,ADest:PFDSet):Boolean;
{Source is the set passed to Select, Dest is the working set to check}
var
 Count:Integer;
begin
 {}
 Result:=False;
 
 {Check Dest}
 if ADest = nil then Exit;
 
 {Clear Dest Set}
 FD_ZERO(ADest^);
 
 {Check Source}
 if ASource <> nil then
  begin
   {Copy Source Set to Dest Set}
   ADest.fd_count:=ASource.fd_count;
   for Count:=ASource.fd_count - 1 downto 0 do
    begin
     ADest.fd_array[Count]:=ASource.fd_array[Count];
    end;
    
   {Clear Source Set}
   FD_ZERO(ASource^);
  end;

 {Return Result}  
 Result:=True;
end;

{==============================================================================}

function TNetworkProtocol.SelectCheck(ASource,ADest:PFDSet;ACode:Integer):Integer;
{Source is the working set to check, Dest is the set passed to Select}
begin
 {Virtual Base Method}
 Result:=0;
end;

{==============================================================================}

function TNetworkProtocol.SelectWait(ASocket:TProtocolSocket;ACode:Integer;ATimeout:LongWord):Integer; 
{Socket is the single socket to check, Code is the type of check, Timeout is how long to wait}
begin
 {Virtual Base Method}
 Result:=0;
end;

{==============================================================================}

function TNetworkProtocol.SendPacket(ASocket:TProtocolSocket;ASource,ADest:Pointer;ASourcePort,ADestPort:Word;APacket:PPacketFragment;ASize,AFlags:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.Accept(ASocket:TProtocolSocket;ASockAddr:PSockAddr;AAddrLength:PInteger):TProtocolSocket;
begin
 {Virtual Base Method}
 Result:=TProtocolSocket(INVALID_SOCKET);
end;

{==============================================================================}

function TNetworkProtocol.Bind(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.CloseSocket(ASocket:TProtocolSocket):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.Connect(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;AAddrLength:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.IoctlSocket(ASocket:TProtocolSocket;ACmd:DWORD;var AArg:u_long):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.GetPeerName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.GetSockName(ASocket:TProtocolSocket;var ASockAddr:TSockAddr;var AAddrLength:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.GetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;var AOptLength:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.Listen(ASocket:TProtocolSocket;ABacklog:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.Recv(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.RecvFrom(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AFromAddr:TSockAddr;var AFromLength:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.Select(ANfds:Integer;AReadfds,AWritefds,AExceptfds:PFDSet;ATimeout:PTimeVal):LongInt;
var
 Code:Integer;
 Count:Integer;
 StartTime:Int64;
 Timeout:LongWord;
 Readfds:TFDSet;
 Writefds:TFDSet;
 Exceptfds:TFDSet;
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 
 if not ReaderLock then Exit;
 try
  {$IFDEF PROTOCOL_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Protocol: Select');
  {$ENDIF}
  
  {Check the Parameters}
  NetworkSetLastError(WSAEINVAL);
  if not Assigned(AReadfds) and not Assigned(AWritefds) and not Assigned(AExceptfds) then Exit;
  
  {Setup the Sets}
  if not SelectStart(AReadfds,@Readfds) then Exit;
  if not SelectStart(AWritefds,@Writefds) then Exit;
  if not SelectStart(AExceptfds,@Exceptfds) then Exit;
  
  {Get the Timeout}
  Timeout:=INFINITE;
  if ATimeout <> nil then
   begin
    Timeout:=(ATimeout.tv_sec * 1000) + (ATimeout.tv_usec div 1000);
   end;
   
  {Get the Socket and Code if the select is for a single socket and set}
  Socket:=SelectGet(@Readfds,@Writefds,@Exceptfds,Code);
  if Socket <> nil then
   begin
    {Single Socket in a Single Set}
    {Wait for the Socket or the Timeout}
    Result:=SelectWait(Socket,Code,Timeout);
    if Result = SOCKET_ERROR then Exit;
    if Result > 0 then
     begin
      {Check Code}
      case Code of
       SELECT_READ:FD_SET(TSocket(Socket),AReadfds^);
       SELECT_WRITE:FD_SET(TSocket(Socket),AWritefds^);
       SELECT_ERROR:FD_SET(TSocket(Socket),AExceptfds^);
      end;  
     end; 
   end
  else
   begin 
    {Multiple Sockets or Multiple Sets}
    {Poll the sets until one matches}
    NetworkSetLastError(WSAENOTSOCK);
    StartTime:=GetTickCount64;
    while True do
     begin
      {Check Read Sockets}
      Result:=SelectCheck(@Readfds,AReadfds,SELECT_READ);
      if Result = SOCKET_ERROR then Exit;
      if Result > 0 then
       begin
        Count:=Result;
        
        {Check Write Sockets}
        Result:=SelectCheck(@Writefds,AWritefds,SELECT_WRITE);
        if Result = SOCKET_ERROR then Exit;
        Inc(Count,Result);
        
        {Check Error Sockets}
        Result:=SelectCheck(@Exceptfds,AExceptfds,SELECT_ERROR);
        if Result = SOCKET_ERROR then Exit;
        Inc(Count,Result);
        
        {Return Result}
        Result:=Count;
        Exit;
       end;
      
      {Check Write Sockets}
      Result:=SelectCheck(@Writefds,AWritefds,SELECT_WRITE);
      if Result = SOCKET_ERROR then Exit;
      if Result > 0 then
       begin
        Count:=Result;
        
        {Check Read Sockets}
        Result:=SelectCheck(@Readfds,AReadfds,SELECT_READ);
        if Result = SOCKET_ERROR then Exit;
        Inc(Count,Result);
        
        {Check Error Sockets}
        Result:=SelectCheck(@Exceptfds,AExceptfds,SELECT_ERROR);
        if Result = SOCKET_ERROR then Exit;
        Inc(Count,Result);
        
        {Return Result}
        Result:=Count;
        Exit;
       end;
       
      {Check Error Sockets}
      Result:=SelectCheck(@Exceptfds,AExceptfds,SELECT_ERROR);
      if Result = SOCKET_ERROR then Exit;
      if Result > 0 then
       begin
        Count:=Result;
        
        {Check Read Sockets}
        Result:=SelectCheck(@Readfds,AReadfds,SELECT_READ);
        if Result = SOCKET_ERROR then Exit;
        Inc(Count,Result);
        
        {Check Write Sockets}
        Result:=SelectCheck(@Writefds,AWritefds,SELECT_WRITE);
        if Result = SOCKET_ERROR then Exit;
        Inc(Count,Result);
        
        {Return Result}
        Result:=Count;
        Exit;
       end;
      
      {Check Timeout}
      if Timeout <> INFINITE then
       begin
        if Timeout = 0 then
         begin
          Result:=0;
          Exit;
         end;
        if GetTickCount64 > (StartTime + Timeout) then
         begin                                     
          Result:=0;
          Exit;
         end;
       end;
      
      {Yield}   
      Sleep(0); 
     end;
   end;  
 finally 
  ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkProtocol.Send(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.SendTo(ASocket:TProtocolSocket;var ABuffer;ALength,AFlags:Integer;var AToAddr:TSockAddr;AToLength:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.SetSockOpt(ASocket:TProtocolSocket;ALevel,AOptName:Integer;AOptValue:PChar;AOptLength:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.Shutdown(ASocket:TProtocolSocket;AHow:Integer):Integer;
begin
 {Virtual Base Method}
 Result:=SOCKET_ERROR;
end;

{==============================================================================}

function TNetworkProtocol.Socket(AFamily,AStruct,AProtocol:Integer):TProtocolSocket;
begin
 {Virtual Base Method}
 Result:=TProtocolSocket(INVALID_SOCKET);
end;

{==============================================================================}

function TNetworkProtocol.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkProtocol.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkProtocol.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkProtocol.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkProtocol.GetStatistics:TProtocolStatistics;
begin
 {Virtual Base Method}
 FillChar(Result,SizeOf(TProtocolStatistics),0);
end;

{==============================================================================}

function TNetworkProtocol.GetTransportByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TProtocolTransport;
var
 Transport:TProtocolTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Get Transport}
  Transport:=TProtocolTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if Transport.Handle = AHandle then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;

      {Return Result}
      Result:=Transport;
      Exit;
     end;
     
    {Get Next} 
    Transport:=TProtocolTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkProtocol.GetTransportByFamily(AFamily:Word;ALock:Boolean;AState:LongWord):TProtocolTransport;
var
 Transport:TProtocolTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Get Transport}
  Transport:=TProtocolTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if Transport.Transport.Family = AFamily then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;

      {Return Result}
      Result:=Transport;
      Exit;
     end;
     
    {Get Next} 
    Transport:=TProtocolTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkProtocol.GetTransportByTransport(ATransport:TNetworkTransport;ALock:Boolean;AState:LongWord):TProtocolTransport;
var
 Transport:TProtocolTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Get Transport}
  Transport:=TProtocolTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if Transport.Transport = ATransport then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;

      {Return Result}
      Result:=Transport;
      Exit;
     end;
    
    {Get Next}     
    Transport:=TProtocolTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkProtocol.GetTransportByNext(APrevious:TProtocolTransport;ALock,AUnlock:Boolean;AState:LongWord):TProtocolTransport;
var
 Transport:TProtocolTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Transport:=TProtocolTransport(FTransports.First);
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
    Transport:=TProtocolTransport(APrevious.Next);
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

function TNetworkProtocol.AddTransport(ATransport:TNetworkTransport):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkProtocol.RemoveTransport(ATransport:TNetworkTransport):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkProtocol.GetSocketByNext(APrevious:TProtocolSocket;ALock,AUnlock:Boolean;AState:LongWord):TProtocolSocket;
var
 Socket:TProtocolSocket;
begin
 {}
 FSockets.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Socket:=TProtocolSocket(FSockets.First);
    if Socket <> nil then
     begin
      {Lock Socket}
      if ALock then if AState = NETWORK_LOCK_READ then Socket.ReaderLock else Socket.WriterLock;
      
      {Return Result}
      Result:=Socket;
     end;
   end
  else
   begin
    {Get Next}
    Socket:=TProtocolSocket(APrevious.Next);
    if Socket <> nil then
     begin
      {Lock Socket}
      if ALock then if AState = NETWORK_LOCK_READ then Socket.ReaderLock else Socket.WriterLock;
      
      {Return Result}
      Result:=Socket;
     end;

    {Unlock Previous}
    if AUnlock then if AState = NETWORK_LOCK_READ then APrevious.ReaderUnlock else APrevious.WriterUnlock;
   end;   
 finally 
  FSockets.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkProtocol.CheckSocket(ASocket:TProtocolSocket;ALock:Boolean;AState:LongWord):Boolean;
var
 Socket:TProtocolSocket;
begin
 {}
 FSockets.ReaderLock;
 try
  Result:=False;
  
  {Get Socket}
  Socket:=TProtocolSocket(FSockets.First);
  while Socket <> nil do
   begin
    {Check Socket}
    if Socket = ASocket then
     begin
      {Lock Socket}
      if ALock then if AState = NETWORK_LOCK_READ then Socket.ReaderLock else Socket.WriterLock;
      
      {Return Result}
      Result:=True;
      Exit;
     end;
     
    {Get Next} 
    Socket:=TProtocolSocket(Socket.Next);
   end;
 finally 
  FSockets.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkProtocol.FindSocket(AFamily,AStruct,AProtocol:Word;ALocalAddress,ARemoteAddress:Pointer;ALocalPort,ARemotePort:Word;ABroadcast,AListen,ALock:Boolean;AState:LongWord):TProtocolSocket;
begin
 {Virtual Base Method}
 Result:=nil;
end;

{==============================================================================}

procedure TNetworkProtocol.FlushSockets(All:Boolean);
begin
 {Virtual Base Method}
end;

{==============================================================================}

function TNetworkProtocol.StartProtocol:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkProtocol.StopProtocol:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkProtocol.ProcessProtocol:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkProtocol.ProcessSockets:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkProtocol.ProcessSocket(ASocket:TProtocolSocket):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkProtocol.BindProtocol(ATransport:TNetworkTransport):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkProtocol.UnbindProtocol(ATransport:TNetworkTransport):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkProtocol.CheckTimer:Boolean; 
begin
 {}
 Result:=False;
 
 {Check Timer}
 if FTimer = nil then Exit;

 {Check Timer}
 Result:=FTimer.CheckTimer;
end;

{==============================================================================}

function TNetworkProtocol.ProcessTimer:Boolean; 
begin
 {}
 Result:=False;
 
 {Check Timer}
 if FTimer = nil then Exit;

 {Process Timer}
 Result:=FTimer.ProcessTimer;
end;

{==============================================================================}

function TNetworkProtocol.SendSocket(ASocket:TProtocolSocket):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Timer}
 if FTimer = nil then Exit;

 {Check Socket}
 {if ASocket = nil then Exit;} {Do not check, used by StopProtocol to release waiting thread}
 
 {Send Socket}
 Result:=FTimer.ScheduleSocket(ASocket,0);
end;

{==============================================================================}

function TNetworkProtocol.ScheduleSocket(ASocket:TProtocolSocket;ATimeout:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Timer}
 if FTimer = nil then Exit;
 
 {Check Socket}
 {if ASocket = nil then Exit;} {Do not check}
 
 {Schedule Socket}
 Result:=FTimer.ScheduleSocket(ASocket,ATimeout);
end;

{==============================================================================}

function TNetworkProtocol.UnscheduleSocket(ASocket:TProtocolSocket):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Timer}
 if FTimer = nil then Exit;
 
 {Check Socket}
 {if ASocket = nil then Exit;} {Do not check}
 
 {Unschedule Socket}
 Result:=FTimer.UnscheduleSocket(ASocket);
end;

{==============================================================================}

function TNetworkProtocol.ScheduleSocketItem(ASocket:TProtocolSocket;AItem:PSocketTimerItem;ATimeout:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Timer}
 if FTimer = nil then Exit;
 
 {Check Socket}
 {if ASocket = nil then Exit;} {Do not check}

 {Check Item}
 {if AItem = nil then Exit;} {Do not check}
 
 {Schedule Socket Item}
 Result:=FTimer.ScheduleSocketItem(ASocket,AItem,ATimeout);
end;

{==============================================================================}

function TNetworkProtocol.UnscheduleSocketItem(ASocket:TProtocolSocket;AItem:PSocketTimerItem):Boolean;
begin
 {}
 Result:=False;
 
 {Check Timer}
 if FTimer = nil then Exit;
 
 {Check Socket}
 {if ASocket = nil then Exit;} {Do not check}
 
 {Check Item}
 {if AItem = nil then Exit;} {Do not check}
 
 {Unschedule Socket Item}
 Result:=FTimer.UnscheduleSocketItem(ASocket,AItem);
end;

{==============================================================================}
{==============================================================================}
{TSocketTimer}
constructor TSocketTimer.Create(AProtocol:TNetworkProtocol);
begin
 {}
 inherited Create;
 FProtocol:=AProtocol;
 
 FLock:=MutexCreate;
 FInterval:=0;
 FCheckTimer:=INVALID_HANDLE_VALUE;
 FProcessSemaphore:=INVALID_HANDLE_VALUE;
 
 FCount:=0;
 FMaxCount:=0;
 
 FFirst:=nil;
 FLast:=nil;
end;

{==============================================================================}

destructor TSocketTimer.Destroy; 
begin
 {}
 AcquireLock;
 try
  if FProcessSemaphore <> INVALID_HANDLE_VALUE then SemaphoreDestroy(FProcessSemaphore);
  FProcessSemaphore:=INVALID_HANDLE_VALUE;
  if FCheckTimer <> INVALID_HANDLE_VALUE then TimerDestroy(FCheckTimer);
  FCheckTimer:=INVALID_HANDLE_VALUE;
  
  FFirst:=nil;
  FLast:=nil;
  inherited Destroy;
 finally 
  ReleaseLock; {Cannot destroy Mutex while holding lock} 
  MutexDestroy(FLock);
 end; 
end;

{==============================================================================}

function TSocketTimer.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TSocketTimer.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TSocketTimer.Dequeue(AMax:Integer):TProtocolSocket;
{Get and remove the first socket from the timer list if the key is less than or equal to Max}
{Max: The maximum value of the key for the page to be dequeued}
{Return: Dequeued Socket or nil on failure}
var
 Item:PSocketTimerItem;
begin
 {}
 Result:=nil;

 if not AcquireLock then Exit;
 try
  {Get Item (First)}
  Item:=FFirst;
  if Item <> nil then
   begin
    {Check Key}
    if Item.Key <= AMax then
     begin
      {Remove First}
      FFirst:=Item.Next;
      
      {Check Next}
      if Item.Next = nil then
       begin
        FLast:=nil;
       end
      else
       begin
        Item.Next.Prev:=nil;
       end;
      
      {Check First}
      if FFirst <> nil then
       begin
        {Update Key}
        Inc(FFirst.Key,Item.Key);
       end;
      
      {Decrement Count}
      Dec(FCount);
      
      {Return Result}
      Result:=TProtocolSocket(Item.Socket);
      
      {Check Scheduled}
      if (Result <> nil) and (Result.Scheduled) then
       begin
        Result.Scheduled:=False;
       end;
      
      {Check Item}
      if (Item.Flags and SOCKET_TIMER_FLAG_DYNAMIC) <> 0 then
       begin
        {Release Item}
        FreeMem(Item);
       end
      else
       begin
        {Update Item}
        Item.Flags:=Item.Flags and not(SOCKET_TIMER_FLAG_ACTIVE);
       end;       
     end;
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSocketTimer.FirstKey:Integer;
{Get the first Key value from the timer list}
{Return: First Key value from timer list or SOCKET_TIMER_KEY_NONE on failure}
var
 Item:PSocketTimerItem;
begin
 {}
 Result:=SOCKET_TIMER_KEY_NONE;
 
 if not AcquireLock then Exit;
 try
  {Get Item (First)}
  Item:=FFirst;
  if Item <> nil then
   begin
    {Return Key}
    Result:=Item.Key;
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSocketTimer.InsertKey(ASocket:TProtocolSocket;AItem:PSocketTimerItem;AKey:Integer):Boolean;
{Insert the supplied socket in the timer list in delta ascending order based on Key}
{Socket: The socket to be inserted}
{Item: Pointer to the socket timer item to insert (Pass nil to allocate dynamically)}
{Key: The key to order the insertion on}
{Return: True if completed or False on failure}
var
 Offset:Integer;
 Item:PSocketTimerItem;
 Prev:PSocketTimerItem;
 Next:PSocketTimerItem;
begin
 {}
 Result:=False;
 
 {Check Key}
 if AKey = SOCKET_TIMER_KEY_NONE then Exit;

 if not AcquireLock then Exit;
 try
  {Check Key}
  if (AKey = 0) and (ASocket <> nil) then
   begin
    if ASocket.Scheduled then Exit;
    
    ASocket.Scheduled:=True;
   end;
 
  {Check Item}
  if AItem <> nil then
   begin
    {Check Flags}
    if (AItem.Flags and SOCKET_TIMER_FLAG_ACTIVE) <> 0 then Exit;
    
    {Get Item}
    Item:=AItem;
    
    {Set Flags}
    Item.Flags:=Item.Flags or SOCKET_TIMER_FLAG_ACTIVE;
   end
  else
   begin  
    {Get Item}
    Item:=GetMem(SizeOf(TSocketTimerItem));
    if Item = nil then Exit;
    
    {Set Flags}
    Item.Flags:=SOCKET_TIMER_FLAG_ACTIVE or SOCKET_TIMER_FLAG_DYNAMIC;
   end; 
   
  {Find Position}
  Offset:=0;
  Prev:=nil;
  Next:=FFirst;
  while Next <> nil do
   begin
    {Delta Ascending}
    if AKey < (Offset + Next.Key) then
     begin
      Dec(Next.Key,(AKey - Offset));
      Break; 
     end;
    Inc(Offset,Next.Key);  
    Prev:=Next;
    Next:=Next.Next; 
   end;
  
  {Insert Item}
  Item.Key:=(AKey - Offset);
  Item.Socket:=ASocket;
  Item.Prev:=Prev;
  Item.Next:=Next;
  
  {Check Prev}
  if Prev = nil then
   begin
    FFirst:=Item;
    
    {Check Next}
    if Next = nil then
     begin
      FLast:=Item;
     end
    else
     begin
      Next.Prev:=Item;
     end;      
   end
  else
   begin
    Prev.Next:=Item;
    
    {Check Next}
    if Next = nil then
     begin
      FLast:=Item;
     end
    else
     begin
      Next.Prev:=Item;
     end;      
   end;
  
  {Increment Count}
  Inc(FCount);
  if FCount > FMaxCount then FMaxCount:=FCount;
  
  {Return Result} 
  Result:=True;       
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSocketTimer.DeleteKey(ASocket:TProtocolSocket;AItem:PSocketTimerItem):Boolean;
{Delete the supplied socket from the timer list}
{Socket: The socket to be deleted}
{Item: Pointer to the socket timer item to be deleted (Pass nil if item was dynamically allocated)}
{Return: True if completed or False on failure}
var
 Item:PSocketTimerItem;
 Prev:PSocketTimerItem;
 Next:PSocketTimerItem;
begin
 {}
 Result:=False;

 if not AcquireLock then Exit;
 try
  {Check Item}
  if AItem <> nil then
   begin
    {Check Flags}
    if (AItem.Flags and SOCKET_TIMER_FLAG_ACTIVE) = 0 then Exit;
   
    {Get Item}
    Item:=AItem;
   end
  else
   begin  
    {Find Item}
    Item:=FFirst;
    while Item <> nil do
     begin
      if Item.Socket = ASocket then
       begin
        Break;
       end;
       
      Item:=Item.Next;
     end;
   end;
   
  {Check Item}
  if Item <> nil then
   begin
    {Delete Element}
    {Get Prev/Next}
    Prev:=Item.Prev;
    Next:=Item.Next;
    {Check Prev}
    if Prev = nil then
     begin
      FFirst:=Next;
      
      {Check Next}
      if Next = nil then
       begin
        FLast:=nil;
       end
      else
       begin
        Next.Prev:=nil;
       end;    
     end
    else
     begin
      Prev.Next:=Next;
      
      {Check Next}
      if Next = nil then
       begin
        FLast:=Prev;
       end
      else
       begin
        Next.Prev:=Prev;
       end;
     end;  
     
    {Check Next}
    if Next <> nil then
     begin
      {Update Key}
      Inc(Next.Key,Item.Key);
     end;
     
    {Decrement Count}
    Dec(FCount);
    
    {Check Scheduled}
    if (Item.Socket <> nil) and (TProtocolSocket(Item.Socket).Scheduled) then
     begin
      TProtocolSocket(Item.Socket).Scheduled:=False;
     end;
     
    {Check Item}
    if (Item.Flags and SOCKET_TIMER_FLAG_DYNAMIC) <> 0 then
     begin
      {Release Item}
      FreeMem(Item);
     end
    else
     begin
      {Update Item}
      Item.Flags:=Item.Flags and not(SOCKET_TIMER_FLAG_ACTIVE);
     end;       
    
    {Return Result} 
    Result:=True;       
   end; 
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSocketTimer.DecrementKey:Integer;
{Decrement the first Key value in the Timer list}
{Return: First Key value in timer list after decrement or SOCKET_TIMER_KEY_NONE on failure}
var
 Item:PSocketTimerItem;
begin
 {}
 Result:=SOCKET_TIMER_KEY_NONE;
 
 if not AcquireLock then Exit;
 try
  {Get Item (First)}
  Item:=FFirst;
  if Item <> nil then
   begin
    {Decrement Key}
    if Item.Key > (SOCKET_TIMER_KEY_MIN + FInterval) then
     begin
      Dec(Item.Key,FInterval);
     end;
         
    {Return Result}
    Result:=Item.Key;
   end;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSocketTimer.StartTimer(AInterval:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check Protocol}
  if FProtocol = nil then Exit;
  
  {Check Interval}
  if AInterval < 1 then Exit;
  
  {Check Timer/Semaphore}
  if FCheckTimer <> INVALID_HANDLE_VALUE then Exit;
  if FProcessSemaphore <> INVALID_HANDLE_VALUE then Exit;
  
  {Set Interval}
  FInterval:=AInterval;
  
  {Create Process Semaphore}
  FProcessSemaphore:=SemaphoreCreate(0);
  if FProcessSemaphore = INVALID_HANDLE_VALUE then Exit;
  
  {Create Check Timer}
  FCheckTimer:=TimerCreateEx(FInterval,TIMER_STATE_ENABLED,TIMER_FLAG_RESCHEDULE or TIMER_FLAG_PRIORITY,TTimerEvent(ProtocolCheckTimer),FProtocol); {Rescheduled Automatically}
  if FCheckTimer = INVALID_HANDLE_VALUE then
   begin
    {Destroy Process Semaphore}
    SemaphoreDestroy(FProcessSemaphore);
    FProcessSemaphore:=INVALID_HANDLE_VALUE;
    
    Exit;
   end; 
 
  {Return Result}
  Result:=True;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}

function TSocketTimer.StopTimer:Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  {Check Timer/Semaphore}
  if FCheckTimer = INVALID_HANDLE_VALUE then Exit;
  if FProcessSemaphore = INVALID_HANDLE_VALUE then Exit;
  
  {Destroy Check Timer}
  if TimerDestroy(FCheckTimer) <> ERROR_SUCCESS then Exit;
  FCheckTimer:=INVALID_HANDLE_VALUE;

  {Destroy Process Semaphore}
  if SemaphoreDestroy(FProcessSemaphore) <> ERROR_SUCCESS then Exit;
  FProcessSemaphore:=INVALID_HANDLE_VALUE;
  
  {Reset Interval}
  FInterval:=0;
  
  {Return Result}
  Result:=True;
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
   
function TSocketTimer.CheckTimer:Boolean;
begin
 {}
 Result:=False;

 {Decrement Key}
 if DecrementKey <= 0 then
  begin
   {Signal Semaphore}
   Result:=(SemaphoreSignal(FProcessSemaphore) = ERROR_SUCCESS);
  end;
end;

{==============================================================================}
   
function TSocketTimer.ProcessTimer:Boolean;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=False;

 {Check Protocol}
 if FProtocol = nil then Exit;
 
 {Wait Semaphore}
 if SemaphoreWait(FProcessSemaphore) = ERROR_SUCCESS then
  begin
   {Dequeue Socket}
   Socket:=Dequeue(0);
   while Socket <> nil do
    begin
     {Process Socket}
     Result:=FProtocol.ProcessSocket(Socket);
     if not Result then Exit;
      
     {Yield}
     Sleep(0);
     
     {Dequeue Socket}
     Socket:=Dequeue(0);
    end;
  end;
end;

{==============================================================================}

function TSocketTimer.ScheduleSocket(ASocket:TProtocolSocket;ATimeout:LongWord):Boolean; 
begin
 {}
 Result:=False;

 {Check Socket}
 {if ASocket = nil then Exit;} {Do not check}

 {Check Timeout} 
 if ATimeout < 1 then
  begin
   {Insert Key (Immediate)}
   Result:=InsertKey(ASocket,nil,0);
  end
 else
  begin
   {Insert Key (Scheduled)}
   Result:=InsertKey(ASocket,nil,ATimeout + FInterval); {Allow one extra interval to account for first decrement}
  end;  
end;

{==============================================================================}

function TSocketTimer.UnscheduleSocket(ASocket:TProtocolSocket):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Socket}
 {if ASocket = nil then Exit;} {Do not check}

 {Delete Key}
 Result:=DeleteKey(ASocket,nil);
end; 
 
{==============================================================================}

function TSocketTimer.ScheduleSocketItem(ASocket:TProtocolSocket;AItem:PSocketTimerItem;ATimeout:LongWord):Boolean;
begin
 {}
 Result:=False;

 {Check Socket}
 {if ASocket = nil then Exit;} {Do not check}

 {Check Item}
 {if AItem = nil then Exit;} {Do not check}
 
 {Check Timeout} 
 if ATimeout < 1 then
  begin
   {Insert Key (Immediate)}
   Result:=InsertKey(ASocket,AItem,0);
  end
 else
  begin
   {Insert Key (Scheduled)}
   Result:=InsertKey(ASocket,AItem,ATimeout + FInterval); {Allow one extra interval to account for first decrement}
  end;  
end;

{==============================================================================}

function TSocketTimer.UnscheduleSocketItem(ASocket:TProtocolSocket;AItem:PSocketTimerItem):Boolean;
begin
 {}
 Result:=False;
 
 {Check Socket}
 {if ASocket = nil then Exit;} {Do not check}

 {Check Item}
 {if AItem = nil then Exit;} {Do not check}
 
 {Delete Key}
 Result:=DeleteKey(ASocket,AItem);
end; 
 
{==============================================================================}
{==============================================================================}
{TSocketThread}
constructor TSocketThread.Create(AProtocol:TNetworkProtocol);
begin
 {}
 inherited Create(True,THREAD_STACK_DEFAULT_SIZE);
 FProtocol:=AProtocol;
end;

{==============================================================================}

destructor TSocketThread.Destroy; 
begin
 {}
 inherited Destroy;
end;

{==============================================================================}

procedure TSocketThread.Execute; 
var
 WorkBuffer:String;
begin
 {}
 try
  {Get Name}
  WorkBuffer:=PROTOCOL_THREAD_NAME;
  if FProtocol <> nil then WorkBuffer:=WorkBuffer + ' (' + FProtocol.Name + ')';
 
  {Set Name}
  ThreadSetName(GetCurrentThreadID,WorkBuffer);
  
  {Set Priority}
  ThreadSetPriority(GetCurrentThreadID,PROTOCOL_THREAD_PRIORITY);
  
  while not(Terminated) do
   begin
    {Check Protocol}
    if FProtocol <> nil then
     begin
      {Process Timer}
      FProtocol.ProcessTimer;
     end;
   end;  
 except
  on E: Exception do
   begin
    if NETWORK_LOG_ENABLED then NetworkLogError(nil,'SocketThread: Exception: ' + E.Message + ' at ' + PtrToHex(ExceptAddr));
   end;
 end; 
end;

{==============================================================================}

function TSocketThread.SendSocket(ASocket:TProtocolSocket):Boolean;
begin
 {}
 Result:=False;
 
 {Check Protocol}
 if FProtocol = nil then Exit;
 
 {Check Socket}
 {if ASocket = nil then Exit;} {Do not check, used by StopProtocol to release waiting thread}
 
 {Send Socket}
 Result:=FProtocol.SendSocket(ASocket);
end;

{==============================================================================}
{==============================================================================}
{TProtocolPort}
constructor TProtocolPort.Create;
begin
 {}
 inherited Create;
 FLock:=MutexCreate;
 
 Port:=IPPORT_ANY;
 Sockets:=TList.Create;
end;

{==============================================================================}

destructor TProtocolPort.Destroy;
begin
 {}
 AcquireLock;
 try
  Sockets.Free;
  inherited Destroy;
 finally 
  ReleaseLock; {Cannot destroy Mutex while holding lock} 
  MutexDestroy(FLock);
 end;
end;

{==============================================================================}

function TProtocolPort.AcquireLock:Boolean;
begin
 {}
 Result:=(MutexLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtocolPort.ReleaseLock:Boolean;
begin
 {}
 Result:=(MutexUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TProtocolSocket}
constructor TProtocolSocket.Create(AProtocol:TNetworkProtocol;ATransport:TNetworkTransport);
begin
 {}
 inherited Create(ATransport);
 FSocketChange:=EventCreate(True,False);

 FProtocol:=AProtocol;
 
 {Set Socket Type}
 if FProtocol <> nil then FStruct:=FProtocol.SocketType;
 
 {Set Protocol Type}
 if FProtocol <> nil then FProto:=FProtocol.Protocol;

 FProtocolState:=nil;
 FProtocolOptions:=nil;
end;

{==============================================================================}

destructor TProtocolSocket.Destroy;
begin
 {}
 WriterLock;
 try
  FProtocolOptions:=nil;
  FProtocolState:=nil;

  FProtocol:=nil;
  
  EventDestroy(FSocketChange);
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  inherited Destroy;
 end; 
end;

{==============================================================================}

function TProtocolSocket.CheckSocket(ASocket:TProtocolSocket;ALock:Boolean;AState:LongWord):Boolean; 
begin
 {}
 Result:=False;
 
 {Check Protocol}
 if FProtocol = nil then Exit;
 
 {Check Socket}
 Result:=FProtocol.CheckSocket(ASocket,ALock,AState);
end;

{==============================================================================}

function TProtocolSocket.WaitChange:Boolean;
begin
 {}
 {Wait Event}
 Result:=(EventWaitEx(FSocketChange,INFINITE) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtocolSocket.WaitChangeEx(ATimeout:LongWord):Boolean;
var
 Status:LongWord;
begin
 {}
 {Check Timeout}
 if ATimeout = 0 then ATimeout:=INFINITE;
 
 {Wait Event}
 Status:=EventWaitEx(FSocketChange,ATimeout);
 Result:=(Status = ERROR_SUCCESS) or ((ATimeout <> INFINITE) and (Status = ERROR_WAIT_TIMEOUT));
end;

{==============================================================================}

function TProtocolSocket.SignalChange:Boolean;
begin
 {}
 {Signal Event}
 Result:=(EventPulse(FSocketChange) = ERROR_SUCCESS);
    
 {Reset Event (Manual Reset)}
 Result:=Result and (EventReset(FSocketChange) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtocolSocket.SendSocket:Boolean;
begin
 {}
 Result:=False;
 
 {Check Protocol}
 if FProtocol = nil then Exit;
 
 {Send Socket}
 Result:=FProtocol.SendSocket(Self);
end;

{==============================================================================}

function TProtocolSocket.ScheduleSocket(ATimeout:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Protocol}
 if FProtocol = nil then Exit;
 
 {Schedule Socket}
 Result:=FProtocol.ScheduleSocket(Self,ATimeout);
end;

{==============================================================================}

function TProtocolSocket.UnscheduleSocket:Boolean; 
begin
 {}
 Result:=False;
 
 {Check Protocol}
 if FProtocol = nil then Exit;
 
 {Unschedule Socket}
 Result:=FProtocol.UnscheduleSocket(Self);
end;

{==============================================================================}

function TProtocolSocket.ScheduleSocketItem(AItem:PSocketTimerItem;ATimeout:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Protocol}
 if FProtocol = nil then Exit;
 
 {Schedule Socket Item}
 Result:=FProtocol.ScheduleSocketItem(Self,AItem,ATimeout);
end;

{==============================================================================}

function TProtocolSocket.UnscheduleSocketItem(AItem:PSocketTimerItem):Boolean;
begin
 {}
 Result:=False;
 
 {Check Protocol}
 if FProtocol = nil then Exit;
 
 {Unschedule Socket Item}
 Result:=FProtocol.UnscheduleSocketItem(Self,AItem);
end;

{==============================================================================}
{==============================================================================}
{TProtocolState}
constructor TProtocolState.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FLocalPort:=IPPORT_ANY;
 FRemotePort:=IPPORT_ANY;
end;

{==============================================================================}

destructor TProtocolState.Destroy;
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

function TProtocolState.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtocolState.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

procedure TProtocolState.SetLocalPort(ALocalPort:Word);
begin
 {}
 if not AcquireLock then Exit;

 FLocalPort:=ALocalPort;

 ReleaseLock;
end;

{==============================================================================}

procedure TProtocolState.SetRemotePort(ARemotePort:Word);
begin
 {}
 if not AcquireLock then Exit;

 FRemotePort:=ARemotePort;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TProtocolOptions}
constructor TProtocolOptions.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
end;

{==============================================================================}

destructor TProtocolOptions.Destroy;
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

function TProtocolOptions.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TProtocolOptions.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TFilterTransport}
constructor TFilterTransport.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 Handle:=INVALID_HANDLE_VALUE;
 Protocol:=IPPROTO_IP;
 Transport:=nil;
end;

{==============================================================================}

destructor TFilterTransport.Destroy;
begin
 {}
 WriterLock;
 try
  Transport:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TFilterTransport.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TFilterTransport.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TFilterTransport.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TFilterTransport.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TNetworkFilter}
constructor TNetworkFilter.Create(AManager:TProtocolManager);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 FManager:=AManager;
 FProtocol:=IPPROTO_IP;
 FTransports:=TNetworkList.Create;
 if FManager <> nil then FManager.AddFilter(Self);
end;

{==============================================================================}

destructor TNetworkFilter.Destroy;
begin
 {}
 WriterLock;
 try
  if FManager <> nil then FManager.RemoveFilter(Self);
  FTransports.Free;
  FProtocol:=IPPROTO_IP;
  FManager:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TNetworkFilter.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkFilter.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkFilter.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkFilter.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkFilter.GetTransportByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TFilterTransport;
var
 Transport:TFilterTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Get Transport} 
  Transport:=TFilterTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if Transport.Handle = AHandle then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;
      
      {Return Result}
      Result:=Transport;
      Exit;
     end;
    
    {Get Next}
    Transport:=TFilterTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkFilter.GetTransportByFamily(AFamily:Word;ALock:Boolean;AState:LongWord):TFilterTransport;
var
 Transport:TFilterTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Get Transport} 
  Transport:=TFilterTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if Transport.Transport.Family = AFamily then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;

      {Return Result}
      Result:=Transport;
      Exit;
     end;
     
    {Get Next}
    Transport:=TFilterTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkFilter.GetTransportByTransport(ATransport:TNetworkTransport;ALock:Boolean;AState:LongWord):TFilterTransport;
var
 Transport:TFilterTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Get Transport} 
  Transport:=TFilterTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if Transport.Transport = ATransport then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;

      {Return Result}
      Result:=Transport;
      Exit;
     end;
     
    {Get Next}
    Transport:=TFilterTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkFilter.GetTransportByNext(APrevious:TFilterTransport;ALock,AUnlock:Boolean;AState:LongWord):TFilterTransport;
var
 Transport:TFilterTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Transport:=TFilterTransport(FTransports.First);
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
    Transport:=TFilterTransport(APrevious.Next);
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

function TNetworkFilter.AddTransport(ATransport:TNetworkTransport):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkFilter.RemoveTransport(ATransport:TNetworkTransport):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkFilter.StartFilter:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkFilter.StopFilter:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkFilter.ProcessFilter:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkFilter.BindFilter(ATransport:TNetworkTransport):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkFilter.UnbindFilter(ATransport:TNetworkTransport):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}
{==============================================================================}
{TConfigTransport}
constructor TConfigTransport.Create;
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 Handle:=INVALID_HANDLE_VALUE;
 ConfigType:=CONFIG_TYPE_AUTO;
 Transport:=nil;
end;

{==============================================================================}

destructor TConfigTransport.Destroy;
begin
 {}
 WriterLock;
 try
  Transport:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TConfigTransport.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TConfigTransport.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TConfigTransport.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TConfigTransport.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
{==============================================================================}
{TNetworkConfig}
 constructor TNetworkConfig.Create(AManager:TProtocolManager);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 FManager:=AManager;
 FConfigType:=CONFIG_TYPE_AUTO;
 FInitDelay:=0;
 FRetryCount:=1;
 FRetryTimeout:=5000;
 FTransports:=TNetworkList.Create;
 if FManager <> nil then FManager.AddConfig(Self);
end;

{==============================================================================}

destructor TNetworkConfig.Destroy;
begin
 {}
 WriterLock;
 try
  if FManager <> nil then FManager.RemoveConfig(Self);
  FTransports.Free;
  FConfigType:=CONFIG_TYPE_AUTO;
  FManager:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TNetworkConfig.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkConfig.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkConfig.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkConfig.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkConfig.GetTransportByHandle(AHandle:THandle;ALock:Boolean;AState:LongWord):TConfigTransport;
var
 Transport:TConfigTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Get Transport}
  Transport:=TConfigTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if Transport.Handle = AHandle then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;

      {Return Result}
      Result:=Transport;
      Exit;
     end;
     
    {Get Next} 
    Transport:=TConfigTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkConfig.GetTransportByFamily(AFamily:Word;ALock:Boolean;AState:LongWord):TConfigTransport;
var
 Transport:TConfigTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Get Transport}
  Transport:=TConfigTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if Transport.Transport.Family = AFamily then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;

      {Return Result}
      Result:=Transport;
      Exit;
     end;
     
    {Get Next} 
    Transport:=TConfigTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkConfig.GetTransportByTransport(ATransport:TNetworkTransport;ALock:Boolean;AState:LongWord):TConfigTransport;
var
 Transport:TConfigTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Get Transport}
  Transport:=TConfigTransport(FTransports.First);
  while Transport <> nil do
   begin
    {Check Transport}
    if Transport.Transport = ATransport then
     begin
      {Lock Transport} 
      if ALock then if AState = NETWORK_LOCK_READ then Transport.ReaderLock else Transport.WriterLock;

      {Return Result}
      Result:=Transport;
      Exit;
     end;
     
    {Get Next} 
    Transport:=TConfigTransport(Transport.Next);
   end;
 finally 
  FTransports.ReaderUnlock;
 end; 
end;

{==============================================================================}

function TNetworkConfig.GetTransportByNext(APrevious:TConfigTransport;ALock,AUnlock:Boolean;AState:LongWord):TConfigTransport;
var
 Transport:TConfigTransport;
begin
 {}
 FTransports.ReaderLock;
 try
  Result:=nil;
  
  {Check Previous}
  if APrevious = nil then
   begin
    {Get First}
    Transport:=TConfigTransport(FTransports.First);
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
    Transport:=TConfigTransport(APrevious.Next);
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

function TNetworkConfig.AddTransport(ATransport:TNetworkTransport):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkConfig.RemoveTransport(ATransport:TNetworkTransport):Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkConfig.StartConfig:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkConfig.StopConfig:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkConfig.ProcessConfig:Boolean;
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkConfig.BindConfig(ATransport:TNetworkTransport):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkConfig.UnbindConfig(ATransport:TNetworkTransport):Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkConfig.SetConfig(AInitDelay,ARetryCount,ARetryTimeout:LongWord):Boolean;
begin
 {Virtual Base Method}
 Result:=True;
 FInitDelay:=AInitDelay;
 FRetryCount:=ARetryCount;
 FRetryTimeout:=ARetryTimeout;
end;

{==============================================================================}
{==============================================================================}
{TNetworkClient}
constructor TNetworkClient.Create(AProtocol:TNetworkProtocol);
begin
 {}
 inherited Create;
 FLock:=SynchronizerCreate;
 
 FProtocol:=AProtocol;
end;

{==============================================================================}

destructor TNetworkClient.Destroy;
begin
 {}
 WriterLock;
 try
  FProtocol:=nil;
  inherited Destroy;
 finally 
  {WriterUnlock;} {Can destroy Synchronizer while holding lock}
  SynchronizerDestroy(FLock);
 end;
end;

{==============================================================================}

function TNetworkClient.ReaderLock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkClient.ReaderUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerReaderUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkClient.WriterLock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkClient.WriterUnlock:Boolean;
begin
 {}
 Result:=(SynchronizerWriterUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TNetworkClient.StartClient:Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}

function TNetworkClient.StopClient:Boolean; 
begin
 {Virtual Base Method}
 Result:=False;
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ProtocolInit;
begin
 {}
 {Check Initialized}
 if ProtocolInitialized then Exit;

 {Create Protocol Manager}
 ProtocolManager:=TProtocolManager.Create(NetworkSettings,TransportManager);
 
 ProtocolInitialized:=True;
end;

{==============================================================================}
 
function ProtocolStart:LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if ProtocolStarted then Exit;
 
 Result:=ERROR_INVALID_PARAMETER;

 {Check Manager}
 if ProtocolManager = nil then Exit;
 
 {Start Protocols}
 if not ProtocolManager.StartProtocols then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to start one or more network protocols');
  end;

 {Start Filters}
 if not ProtocolManager.StartFilters then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to start one or more network filters');
  end;
  
 {Start Configs}
 if not ProtocolManager.StartConfigs then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to start one or more network configs');
  end;
  
 //To Do //Clients
 
 {Set Started} 
 ProtocolStarted:=True;
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ProtocolStop:LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if not(ProtocolStarted) then Exit;
 
 Result:=ERROR_INVALID_PARAMETER;

 {Check Manager}
 if ProtocolManager = nil then Exit;
 
 //To Do //Clients
 
 {Stop Configs}
 if not ProtocolManager.StopConfigs then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to stop one or more network configs');
  end;

 {Stop Filters}
 if not ProtocolManager.StopFilters then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to stop one or more network filters');
  end;
  
 {Stop Protocols}
 if not ProtocolManager.StopProtocols then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to stop one or more network protocols');
  end;

 {Set Started}
 ProtocolStarted:=False;    
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{Protocol Functions}
 
{==============================================================================}
{==============================================================================}
{Protocol Helper Functions}
procedure ProtocolCheckTimer(Data:Pointer);
var
 Protocol:TNetworkProtocol;
begin
 {}
 {Check Data}
 if Data = nil then Exit;
 
 {Get Protocol}
 Protocol:=TNetworkProtocol(Data);
 
 {Check Timer}
 Protocol.CheckTimer;
end;
 
{==============================================================================}
{==============================================================================}

initialization
 ProtocolInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.