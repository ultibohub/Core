{
Ultibo Network Sockets interface unit.

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

 
Network Sockets
===============

 Notes: All BSD functions that accept an Address or Port expect
        them to be in Network order. All other functions that take
        an Address or Port expect them to be in Host order

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Sockets;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,Platform,Threads,Devices,SysUtils,Classes,Network,Transport,Protocol,Loopback,ARP,IP,IPv6,UDP,TCP,ICMP,ICMPv6,IGMP,RAW,DHCP,DNS;

//To Do //See also: \source\packages\rtl-extra\src\win\sockets.pp
//To Do //See also: \source\packages\rtl-extra\src\inc\sockets.inc
//To Do //See also: \source\packages\rtl-extra\src\inc\socketsh.inc

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
const
 {Sockets specific constants}
 SOCKETS_DEVICE_TIMER_INTERVAL    = 500;

 SOCKETS_CONFIG_TIMER_INTERVAL    = 1000;
 SOCKETS_FILTER_TIMER_INTERVAL    = 1000;
 SOCKETS_PROTOCOL_TIMER_INTERVAL  = 250;  {Previously 100}
 SOCKETS_TRANSPORT_TIMER_INTERVAL = 1000;
 SOCKETS_ADAPTER_TIMER_INTERVAL   = 1000;
 
const
 EsockEINTR            = WSAEINTR;
 EsockEBADF            = WSAEBADF;
 EsockEFAULT           = WSAEFAULT;
 EsockEINVAL           = WSAEINVAL;
 EsockEACCESS          = WSAEACCES;
 EsockEMFILE           = WSAEMFILE;
 EsockEMSGSIZE         = WSAEMSGSIZE;
 EsockENOBUFS          = WSAENOBUFS;
 EsockENOTCONN         = WSAENOTCONN;
 EsockENOTSOCK         = WSAENOTSOCK;
 EsockEPROTONOSUPPORT  = WSAEPROTONOSUPPORT;
 EsockEWOULDBLOCK      = WSAEWOULDBLOCK;
  
 SHUT_RD          = SD_RECEIVE; {Aliases so we are cross-platform}
 SHUT_WR          = SD_SEND;
 SHUT_RDWR        = SD_BOTH;
  
const
 { Socket types }
 SOCK_STREAM     = GlobalSock.SOCK_STREAM;
 SOCK_DGRAM      = GlobalSock.SOCK_DGRAM;
 SOCK_RAW        = GlobalSock.SOCK_RAW;
 SOCK_RDM        = GlobalSock.SOCK_RDM;
 SOCK_SEQPACKET  = GlobalSock.SOCK_SEQPACKET;
  
 INADDR_ANY   = GlobalSock.INADDR_ANY;
 INADDR_NONE  = GlobalSock.INADDR_NONE;
  
const
 { Two constants to determine whether part of soket is for in or output }
 S_IN = 0;
 S_OUT = 1;
  
const
{ Address families. }
 AF_UNSPEC       = GlobalSock.AF_UNSPEC;
 AF_UNIX         = GlobalSock.AF_UNIX;
 AF_INET         = GlobalSock.AF_INET;
 AF_IMPLINK      = GlobalSock.AF_IMPLINK;
 AF_PUP          = GlobalSock.AF_PUP;
 AF_CHAOS        = GlobalSock.AF_CHAOS;
 AF_IPX          = GlobalSock.AF_IPX;
 AF_NS           = GlobalSock.AF_NS;
 AF_ISO          = GlobalSock.AF_ISO;
 AF_OSI          = GlobalSock.AF_OSI;
 AF_ECMA         = GlobalSock.AF_ECMA;
 AF_DATAKIT      = GlobalSock.AF_DATAKIT;
 AF_CCITT        = GlobalSock.AF_CCITT;
 AF_SNA          = GlobalSock.AF_SNA;
 AF_DECnet       = GlobalSock.AF_DECnet;
 AF_DLI          = GlobalSock.AF_DLI;
 AF_LAT          = GlobalSock.AF_LAT;
 AF_HYLINK       = GlobalSock.AF_HYLINK;
 AF_APPLETALK    = GlobalSock.AF_APPLETALK;
 AF_NETBIOS      = GlobalSock.AF_NETBIOS;
 AF_VOICEVIEW    = GlobalSock.AF_VOICEVIEW;
 AF_FIREFOX      = GlobalSock.AF_FIREFOX;
 AF_UNKNOWN1     = GlobalSock.AF_UNKNOWN1;
 AF_BAN          = GlobalSock.AF_BAN;
 AF_ATM          = GlobalSock.AF_ATM;
 AF_INET6        = GlobalSock.AF_INET6;
 AF_CLUSTER      = GlobalSock.AF_CLUSTER;
 AF_12844        = GlobalSock.AF_12844;
 AF_IRDA         = GlobalSock.AF_IRDA;
 AF_NETDES       = GlobalSock.AF_NETDES;

 AF_MAX          = GlobalSock.AF_MAX;

const
{ Protocol families, same as address families for now. }
 PF_UNSPEC       = GlobalSock.PF_UNSPEC;
 PF_UNIX         = GlobalSock.PF_UNIX;
 PF_INET         = GlobalSock.PF_INET;
 PF_IMPLINK      = GlobalSock.PF_IMPLINK;
 PF_PUP          = GlobalSock.PF_PUP;
 PF_CHAOS        = GlobalSock.PF_CHAOS;
 PF_NS           = GlobalSock.PF_NS;
 PF_IPX          = GlobalSock.PF_IPX;
 PF_ISO          = GlobalSock.PF_ISO;
 PF_OSI          = GlobalSock.PF_OSI;
 PF_ECMA         = GlobalSock.PF_ECMA;
 PF_DATAKIT      = GlobalSock.PF_DATAKIT;
 PF_CCITT        = GlobalSock.PF_CCITT;
 PF_SNA          = GlobalSock.PF_SNA;
 PF_DECnet       = GlobalSock.PF_DECnet;
 PF_DLI          = GlobalSock.PF_DLI;
 PF_LAT          = GlobalSock.PF_LAT;
 PF_HYLINK       = GlobalSock.PF_HYLINK;
 PF_APPLETALK    = GlobalSock.PF_APPLETALK;
 PF_VOICEVIEW    = GlobalSock.PF_VOICEVIEW;
 PF_FIREFOX      = GlobalSock.PF_FIREFOX;
 PF_UNKNOWN1     = GlobalSock.PF_UNKNOWN1;
 PF_BAN          = GlobalSock.PF_BAN;
 PF_ATM          = GlobalSock.PF_ATM;
 PF_INET6        = GlobalSock.PF_INET6;
 PF_CLUSTER      = GlobalSock.PF_CLUSTER;
 PF_12844        = GlobalSock.PF_12844;
 PF_IRDA         = GlobalSock.PF_IRDA;
 PF_NETDES       = GlobalSock.PF_NETDES;

 PF_MAX          = GlobalSock.PF_MAX;
  
const
{ Protocols }
 IPPROTO_IP       =  GlobalSock.IPPROTO_IP;
 IPPROTO_ICMP     =  GlobalSock.IPPROTO_ICMP;
 IPPROTO_IGMP     =  GlobalSock.IPPROTO_IGMP;
 IPPROTO_GGP      =  GlobalSock.IPPROTO_GGP;
 IPPROTO_TCP      =  GlobalSock.IPPROTO_TCP;
 IPPROTO_EGP      =  GlobalSock.IPPROTO_EGP;
 IPPROTO_PUP      =  GlobalSock.IPPROTO_PUP;
 IPPROTO_UDP      =  GlobalSock.IPPROTO_UDP;
 IPPROTO_HMP      =  GlobalSock.IPPROTO_HMP;
 IPPROTO_IDP      =  GlobalSock.IPPROTO_IDP;
 IPPROTO_RDP      =  GlobalSock.IPPROTO_RDP;
 IPPROTO_IPV6	  =  GlobalSock.IPPROTO_IPV6;
 IPPROTO_ROUTING  =  GlobalSock.IPPROTO_ROUTING;
 IPPROTO_FRAGMENT =  GlobalSock.IPPROTO_FRAGMENT;
 IPPROTO_ICMPV6   =  GlobalSock.IPPROTO_ICMPV6;
 IPPROTO_RVD      =  GlobalSock.IPPROTO_RVD;
 IPPROTO_ND       =  GlobalSock.IPPROTO_ND;
 IPPROTO_RAW      =  GlobalSock.IPPROTO_RAW;
 IPPROTO_MAX      =  GlobalSock.IPPROTO_MAX;
  
{ Port/socket numbers: network standard functions}
 IPPORT_ANY     =   GlobalSock.IPPORT_ANY;
 IPPORT_ECHO    =   GlobalSock.IPPORT_ECHO;
 IPPORT_DISCARD =   GlobalSock.IPPORT_DISCARD;
 IPPORT_SYSTAT  =   GlobalSock.IPPORT_SYSTAT;
 IPPORT_DAYTIME =   GlobalSock.IPPORT_DAYTIME;
 IPPORT_NETSTAT =   GlobalSock.IPPORT_NETSTAT;
 IPPORT_FTP     =   GlobalSock.IPPORT_FTP;
 IPPORT_TELNET  =   GlobalSock.IPPORT_TELNET;
 IPPORT_SMTP    =   GlobalSock.IPPORT_SMTP;
 IPPORT_TIMESERVER  =  GlobalSock.IPPORT_TIMESERVER;
 IPPORT_NAMESERVER  =  GlobalSock.IPPORT_NAMESERVER;
 IPPORT_WHOIS       =  GlobalSock.IPPORT_WHOIS;
 IPPORT_DNS         =  GlobalSock.IPPORT_DNS;
 IPPORT_MTP         =  GlobalSock.IPPORT_MTP;
 IPPORT_BOOTPS      =  GlobalSock.IPPORT_BOOTPS;
 IPPORT_BOOTPC      =  GlobalSock.IPPORT_BOOTPC;

{ Port/socket numbers: host specific functions }
 IPPORT_TFTP        =  GlobalSock.IPPORT_TFTP;
 IPPORT_RJE         =  GlobalSock.IPPORT_RJE;
 IPPORT_FINGER      =  GlobalSock.IPPORT_FINGER;
 IPPORT_TTYLINK     =  GlobalSock.IPPORT_TTYLINK;
 IPPORT_SUPDUP      =  GlobalSock.IPPORT_SUPDUP;

{ UNIX TCP sockets }
 IPPORT_EXECSERVER  =  GlobalSock.IPPORT_EXECSERVER;
 IPPORT_LOGINSERVER =  GlobalSock.IPPORT_LOGINSERVER;
 IPPORT_CMDSERVER   =  GlobalSock.IPPORT_CMDSERVER;
 IPPORT_EFSSERVER   =  GlobalSock.IPPORT_EFSSERVER;

{ UNIX UDP sockets }
 IPPORT_BIFFUDP     =  GlobalSock.IPPORT_BIFFUDP;
 IPPORT_WHOSERVER   =  GlobalSock.IPPORT_WHOSERVER;
 IPPORT_ROUTESERVER =  GlobalSock.IPPORT_ROUTESERVER;

{ Ports < IPPORT_RESERVED are reserved for privileged processes (e.g. root). }
 IPPORT_RESERVED    =  GlobalSock.IPPORT_RESERVED;
  
const
{ Options for use with [gs]etsockopt at the IP level. }
 IP_OPTIONS          = GlobalSock.IP_OPTIONS;
 IP_MULTICAST_IF     = GlobalSock.IP_MULTICAST_IF;
 IP_MULTICAST_TTL    = GlobalSock.IP_MULTICAST_TTL;
 IP_MULTICAST_LOOP   = GlobalSock.IP_MULTICAST_LOOP;
 IP_ADD_MEMBERSHIP   = GlobalSock.IP_ADD_MEMBERSHIP;
 IP_DROP_MEMBERSHIP  = GlobalSock.IP_DROP_MEMBERSHIP;
 IP_TTL              = GlobalSock.IP_TTL;
 IP_TOS              = GlobalSock.IP_TOS;
 IP_DONTFRAGMENT     = GlobalSock.IP_DONTFRAGMENT;
 IP_HDRINCL          = GlobalSock.IP_HDRINCL;
  
const  
{ Option flags per-socket. }
 SO_DEBUG        = GlobalSock.SO_DEBUG;
 SO_ACCEPTCONN   = GlobalSock.SO_ACCEPTCONN;
 SO_REUSEADDR    = GlobalSock.SO_REUSEADDR;
 SO_KEEPALIVE    = GlobalSock.SO_KEEPALIVE;
 SO_DONTROUTE    = GlobalSock.SO_DONTROUTE;
 SO_BROADCAST    = GlobalSock.SO_BROADCAST;
 SO_USELOOPBACK  = GlobalSock.SO_USELOOPBACK;
 SO_LINGER       = GlobalSock.SO_LINGER;
 SO_OOBINLINE    = GlobalSock.SO_OOBINLINE;

 SO_DONTLINGER  =   GlobalSock.SO_DONTLINGER;

{ Additional options. }
 SO_SNDBUF       = GlobalSock.SO_SNDBUF;
 SO_RCVBUF       = GlobalSock.SO_RCVBUF;
 SO_SNDLOWAT     = GlobalSock.SO_SNDLOWAT;
 SO_RCVLOWAT     = GlobalSock.SO_RCVLOWAT;
 SO_SNDTIMEO     = GlobalSock.SO_SNDTIMEO;
 SO_RCVTIMEO     = GlobalSock.SO_RCVTIMEO;
 SO_ERROR        = GlobalSock.SO_ERROR;
 SO_TYPE         = GlobalSock.SO_TYPE;
 SO_CONNTIMEO    = GlobalSock.SO_CONNTIMEO;

{ Options for connect and disconnect data and options.  Used only by non-TCP/IP transports such as DECNet, OSI TP4, etc. }
 SO_CONNDATA     = GlobalSock.SO_CONNDATA;
 SO_CONNOPT      = GlobalSock.SO_CONNOPT;
 SO_DISCDATA     = GlobalSock.SO_DISCDATA;
 SO_DISCOPT      = GlobalSock.SO_DISCOPT;
 SO_CONNDATALEN  = GlobalSock.SO_CONNDATALEN;
 SO_CONNOPTLEN   = GlobalSock.SO_CONNOPTLEN;
 SO_DISCDATALEN  = GlobalSock.SO_DISCDATALEN;
 SO_DISCOPTLEN   = GlobalSock.SO_DISCOPTLEN;

{ Option for opening sockets for synchronous access. }
 SO_OPENTYPE     = GlobalSock.SO_OPENTYPE;
 SO_SYNCHRONOUS_ALERT    = GlobalSock.SO_SYNCHRONOUS_ALERT;
 SO_SYNCHRONOUS_NONALERT = GlobalSock.SO_SYNCHRONOUS_NONALERT;

{ Other NT-specific options. }
 SO_MAXDG        = GlobalSock.SO_MAXDG;
 SO_MAXPATHDG    = GlobalSock.SO_MAXPATHDG;
 SO_UPDATE_ACCEPT_CONTEXT     = GlobalSock.SO_UPDATE_ACCEPT_CONTEXT;
 SO_CONNECT_TIME = GlobalSock.SO_CONNECT_TIME;

{ TCP options. }
 TCP_NODELAY     = GlobalSock.TCP_NODELAY;
 TCP_MAXSEG      = GlobalSock.TCP_MAXSEG;
 TCP_NOPUSH      = GlobalSock.TCP_NOPUSH;
 TCP_NOOPT       = GlobalSock.TCP_NOOPT;
 TCP_BSDURGENT   = GlobalSock.TCP_BSDURGENT;

 TCP_WSCALE      = GlobalSock.TCP_WSCALE;
 TCP_NOSACK      = GlobalSock.TCP_NOSACK;

{ UDP options. }
 UDP_NOCHECKSUM  = GlobalSock.UDP_NOCHECKSUM;
  
const
{ Level number for (get/set)sockopt() to apply to socket itself. }
 SOL_SOCKET      = GlobalSock.SOL_SOCKET;

{ Maximum queue length specifiable by listen. }
 SOMAXCONN       = GlobalSock.SOMAXCONN;

 MSG_OOB         = GlobalSock.MSG_OOB;
 MSG_PEEK        = GlobalSock.MSG_PEEK;
 MSG_DONTROUTE   = GlobalSock.MSG_DONTROUTE;

 MSG_MAXIOVLEN   = GlobalSock.MSG_MAXIOVLEN;

 MSG_PARTIAL     = GlobalSock.MSG_PARTIAL;
  
{ This is used instead of -1, since the TSocket type is unsigned.}
 INVALID_SOCKET         = GlobalSock.INVALID_SOCKET;
 SOCKET_ERROR           = GlobalSock.SOCKET_ERROR;
  
const
 {Error codes from getaddrinfo()}
 EAI_AGAIN    = GlobalSock.EAI_AGAIN;
 EAI_BADFLAGS = GlobalSock.EAI_BADFLAGS;
 EAI_FAIL     = GlobalSock.EAI_FAIL;
 EAI_FAMILY   = GlobalSock.EAI_FAMILY;
 EAI_MEMORY   = GlobalSock.EAI_MEMORY;
 {EAI_NODATA   = GlobalSock.EAI_NODATA;}
 EAI_NONAME   = GlobalSock.EAI_NONAME;
 EAI_SERVICE  = GlobalSock.EAI_SERVICE;
 EAI_SOCKTYPE = GlobalSock.EAI_SOCKTYPE;

 EAI_NODATA = GlobalSock.EAI_NODATA;
 
const
 {Flags used in "hints" argument to getaddrinfo()}
 {Note: Under Linux these values may be different}
 AI_PASSIVE                = GlobalSock.AI_PASSIVE;
 AI_CANONNAME              = GlobalSock.AI_CANONNAME;
 AI_NUMERICHOST            = GlobalSock.AI_NUMERICHOST;
 AI_NUMERICSERV            = GlobalSock.AI_NUMERICSERV;
 AI_ALL                    = GlobalSock.AI_ALL;
 AI_ADDRCONFIG             = GlobalSock.AI_ADDRCONFIG;
 AI_V4MAPPED               = GlobalSock.AI_V4MAPPED;
 AI_NON_AUTHORITATIVE      = GlobalSock.AI_NON_AUTHORITATIVE;
 AI_SECURE                 = GlobalSock.AI_SECURE;
 AI_RETURN_PREFERRED_NAMES = GlobalSock.AI_RETURN_PREFERRED_NAMES;
 AI_FQDN                   = GlobalSock.AI_FQDN;
 AI_FILESERVER             = GlobalSock.AI_FILESERVER;

 {Flags for getnameinfo()}
 NI_NOFQDN      = GlobalSock.NI_NOFQDN;
 NI_NUMERICHOST = GlobalSock.NI_NUMERICHOST;
 NI_NAMEREQD    = GlobalSock.NI_NAMEREQD;
 NI_NUMERICSERV = GlobalSock.NI_NUMERICSERV;
 NI_DGRAM       = GlobalSock.NI_DGRAM;

 NI_MAXHOST = GlobalSock.NI_MAXHOST;
 NI_MAXSERV = GlobalSock.NI_MAXSERV;

 INET_ADDRSTR_ANY = GlobalSock.INET_ADDRSTR_ANY;
 INET6_ADDRSTR_INIT = GlobalSock.INET6_ADDRSTR_INIT;

 INET_ADDRSTR_BROADCAST = GlobalSock.INET_ADDRSTR_BROADCAST;
 
 INET_ADDRSTRLEN  = GlobalSock.INET_ADDRSTRLEN;
 INET6_ADDRSTRLEN = GlobalSock.INET6_ADDRSTRLEN;

 IN6ADDR_ANY_INIT:TIn6Addr = (u6_addr16: (0, 0, 0, 0, 0, 0, 0, 0));
 IN6ADDR_LOOPBACK_INIT:TIn6Addr = (u6_addr8: (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1));
  
{==============================================================================}
type
 {Sockets specific types}
 PSocketsDeviceEvent = ^TSocketsDeviceEvent;
 TSocketsDeviceEvent = record
  Timer:TTimerHandle;
  Device:PNetworkDevice;
 end;

type
 cushort = Word;
 cuint8 = Byte;
 cuint16 = Word;
 cuint32 = Cardinal;
 size_t = GlobalTypes.SIZE_T; {cuint32;}
 ssize_t = cuint16;
 cint = LongInt;
 pcint = ^cint;
 tsocklen = cint;
 psocklen = ^tsocklen;

type 
 sa_family_t = cushort; 
 
type
 pin_addr = ^in_addr;
 in_addr = GlobalSock.in_addr;

 TIn_addr = in_addr;

 TInAddr = GlobalSock.TInAddr;
 PInAddr = GlobalSock.PInAddr;

 psockaddr_in = ^sockaddr_in;
 sockaddr_in = GlobalSock.sockaddr_in;

 TInetSockAddr = sockaddr_in;
 PInetSockAddr = psockaddr_in;

 psockaddr = GlobalSock.psockaddr;
 sockaddr = GlobalSock.TSockAddr;

 TSockAddr = GlobalSock.TSockAddr;

 plinger = GlobalSock.plinger;
 linger = GlobalSock.linger;
 TLinger = GlobalSock.TLinger;

 pin6_addr = ^in6_addr;
 in6_addr = GlobalSock.in6_addr;

 Tin6_addr = in6_addr;

 TIn6Addr = GlobalSock.TIn6Addr;
 PIn6Addr = GlobalSock.PIn6Addr;

 psockaddr_in6 = ^sockaddr_in6;
 sockaddr_in6 = GlobalSock.sockaddr_in6;

 TInetSockAddr6 = sockaddr_in6;
 PInetSockAddr6 = psockaddr_in6;
 
type 
 TSockPairArray = array[0..1] of Longint;
 TSockArray  = array[1..2] of Longint;              {Legacy}

 psockaddr_un = ^sockaddr_un;
 sockaddr_un = packed record
  sun_family:sa_family_t;
  sun_path:array[0..107] of char;
 end;

 {Tsocket = LongInt;}  {To ease porting code from Kylix libc unit to sockets unit.}
 
type
 {Structure used in getaddrinfo() call}
 PAddrInfo = GlobalSock.PAddrInfo;
 TAddrInfo = GlobalSock.TAddrInfo;
 
{==============================================================================}
{var}
 {Sockets specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure SocketsInit;
function SocketsStart:LongWord;
function SocketsStop:LongWord;

procedure SocketsAsyncStart(Data:Pointer);

{==============================================================================}
{Sockets Functions}
function SocketError: cint;

function Socket(Domain,SocketType,Protocol:Longint):Longint;
function Send(Sock:Longint;const Buf;BufLen,Flags:Longint):Longint;
function SendTo(Sock:Longint;const Buf;BufLen,Flags:Longint;var Addr; AddrLen : Longint):Longint;
function Recv(Sock:Longint;var Buf;BufLen,Flags:Longint):Longint;
function RecvFrom(Sock : Longint; var Buf; Buflen,Flags : Longint; var Addr; var AddrLen : longint) : longint;
function Connect(Sock:Longint;const Addr;Addrlen:Longint):Boolean;
function Shutdown(Sock:Longint;How:Longint):Longint;
function Bind(Sock:Longint;const Addr;AddrLen:Longint):Boolean;
function Listen(Sock,MaxConnect:Longint):Boolean;
function Accept(Sock:Longint;var Addr;var Addrlen:Longint):Longint;
function GetSocketName(Sock:Longint;var Addr;var Addrlen:Longint):Longint;
function GetPeerName(Sock:Longint;var Addr;var Addrlen:Longint):Longint;
function GetSocketOptions(Sock,Level,OptName:Longint;var OptVal;var optlen:longint):Longint;
function SetSocketOptions(Sock,Level,OptName:Longint;const OptVal;optlen:longint):Longint;
function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;

function CloseSocket(Sock:Longint):Longint;

function Inet_Addr(cp: PChar): Longint;
function Inet_Ntoa(inaddr: TInAddr): PChar; 

function Inet_Pton(family: Longint; Source: PChar; Dest: Pointer): Longint;
function Inet_Ntop(family: Longint; Source: Pointer; Dest: PChar; Size: Longint): PChar;

function GetHostByAddr(addr: Pointer; len, family: Longint): PHostEnt; 
function GetHostByName(name: PChar): PHostEnt; 
function GetHostName(name: PChar; len: Longint): Longint; 
function GetNetByAddr(addr: Pointer; len, Struct: Integer): PNetEnt; 
function GetNetByName(name: PChar): PNetEnt; 
function GetServByPort(port: Longint; proto: PChar): PServEnt; 
function GetServByName(name, proto: PChar): PServEnt; 
function GetProtoByNumber(proto: Longint): PProtoEnt; 
function GetProtoByName(name: PChar): PProtoEnt; 

function GetAddrInfo(HostName, ServName: PChar; Hints: PAddrInfo; var Addr: PAddrInfo): Longint;
procedure FreeAddrInfo(ai: PAddrInfo);
function GetNameInfo(sa: PSockAddr; salen: Longint; host: PChar; hostlen: DWORD; serv: PChar; servlen: DWORD; flags: Longint): Longint;

{==============================================================================}
{RTL Sockets Functions}
function fpsocket(domain:cint; xtype:cint; protocol: cint):cint;
function fpsend(s:cint; msg:pointer; len:size_t; flags:cint):ssize_t;
function fpsendto(s:cint; msg:pointer; len:size_t; flags:cint; tox :psockaddr; tolen: tsocklen):ssize_t;
function fprecv(s:cint; buf: pointer; len: size_t; flags: cint):ssize_t;
function fprecvfrom(s:cint; buf: pointer; len: size_t; flags: cint; from : psockaddr; fromlen : psocklen):ssize_t;
function fpconnect(s:cint; name  : psockaddr; namelen : tsocklen):cint;
function fpshutdown(s:cint; how:cint):cint;
function fpbind(s:cint; addrx : psockaddr; addrlen : tsocklen):cint;
function fplisten(s:cint; backlog : cint):cint;
function fpaccept(s:cint; addrx : psockaddr; addrlen : psocklen):cint;
function fpgetsockname(s:cint; name  : psockaddr; namelen : psocklen):cint;
function fpgetpeername(s:cint; name  : psockaddr; namelen : psocklen):cint;
function fpgetsockopt(s:cint; level:cint; optname:cint; optval:pointer; optlen : psocklen):cint;
function fpsetsockopt(s:cint; level:cint; optname:cint; optval:pointer; optlen :tsocklen):cint;
function fpsocketpair(d:cint; xtype:cint; protocol:cint; sv:pcint):cint;

{==============================================================================}
{Sockets Helper Functions}
procedure SocketsProcessConfig(Data:Pointer);
procedure SocketsProcessFilter(Data:Pointer);
procedure SocketsProcessProtocol(Data:Pointer);
procedure SocketsProcessTransport(Data:Pointer);
procedure SocketsProcessAdapter(Data:Pointer);

procedure SocketsNetworkDeviceAdd(Event:PSocketsDeviceEvent);
function SocketsNetworkDeviceRemove(Network:PNetworkDevice):LongWord;

function SocketsNetworkDeviceEnum(Network:PNetworkDevice;Data:Pointer):LongWord;
function SocketsNetworkDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Sockets specific variables}
 SocketsInitialized:Boolean;
 
 SocketsLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;

 SocketsTlsSize:LongWord;
 SocketsTlsIndex:LongWord;
 
 SocketsConfigTimer:TTimerHandle = INVALID_HANDLE_VALUE;
 SocketsFilterTimer:TTimerHandle = INVALID_HANDLE_VALUE;
 SocketsProtocolTimer:TTimerHandle = INVALID_HANDLE_VALUE;
 SocketsTransportTimer:TTimerHandle = INVALID_HANDLE_VALUE;
 SocketsAdapterTimer:TTimerHandle = INVALID_HANDLE_VALUE;
 
 SocketsStartupCount:LongWord;
 SocketsStartupError:LongWord;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure SocketsInit;
begin
 {}
 {Check Initialized}
 if SocketsInitialized then Exit;
 
 {Create Lock}
 SocketsLock:=CriticalSectionCreate;
 if SocketsLock = INVALID_HANDLE_VALUE then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to create socket lock');
  end;

 {Set TLS Size}
 SocketsTlsSize:=SizeOf(TNetToAddr);
 
 {Allocate TLS Index}
 SocketsTlsIndex:=ThreadAllocTlsIndexEx(THREAD_TLS_FLAG_FREE);
 if SocketsTlsIndex = TLS_OUT_OF_INDEXES then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to allocate TLS index');
  end;
 
 {Set Startup Defaults}
 SocketsStartupCount:=0;
 SocketsStartupError:=ERROR_NOT_READY;
 
 SocketsInitialized:=True;
end;

{==============================================================================}

function SocketsStart:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Managers}
 if AdapterManager = nil then Exit;
 if TransportManager = nil then Exit;
 if ProtocolManager = nil then Exit;

 {Check Clients}
 if DNSClient = nil then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(SocketsLock) = ERROR_SUCCESS then
  begin
   try
    {Check Count}
    if SocketsStartupCount > 0 then
     begin
      {$IFDEF SOCKET_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Additional start call');
      {$ENDIF}
 
      {Increment Count}
      Inc(SocketsStartupCount);
 
      {Return Result}
      Result:=SocketsStartupError; 
     end
    else
     begin
      {$IFDEF SOCKET_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Initial start call');
      {$ENDIF}
 
      {Increment Count}
      Inc(SocketsStartupCount);
 
      {Initialize Components}
      Result:=ERROR_OPERATION_FAILED;
      SocketsStartupError:=ERROR_OPERATION_FAILED;
 
      {Enumerate Adapters}
      NetworkDeviceEnumerate(SocketsNetworkDeviceEnum,nil);
      
      {Start Adapters}
      if NetworkStart <> ERROR_SUCCESS then Exit;
      {$IFDEF SOCKET_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Started adapters');
      {$ENDIF}
       
      {Start Transports}
      if TransportStart <> ERROR_SUCCESS then Exit;
      {$IFDEF SOCKET_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Started transports');
      {$ENDIF}
       
      {Start Protocols}
      if ProtocolStart <> ERROR_SUCCESS then Exit;
      {$IFDEF SOCKET_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Started protocols');
      {$ENDIF}
       
      {Start Clients}
      if DNSStart <> ERROR_SUCCESS then Exit;
      {$IFDEF SOCKET_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Started clients');
      {$ENDIF}
       
      {Bind Transports} //To Do //Move to TransportBind ?
      if not TransportManager.BindTransports(nil) then
       begin
        if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to bind one or more network transports');
       end;
      
      //To Do //Bind Protocols ? //No ?
      
      //To Do //Bind Clients ? //No ?
      
      {Create Config Timer}
      SocketsConfigTimer:=TimerCreateEx(SOCKETS_CONFIG_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsProcessConfig),nil); {Rescheduled by Timer Event}

      {Create Filter Timer}
      SocketsFilterTimer:=TimerCreateEx(SOCKETS_FILTER_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsProcessFilter),nil); {Rescheduled by Timer Event}

      {Create Protocol Timer}
      SocketsProtocolTimer:=TimerCreateEx(SOCKETS_PROTOCOL_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsProcessProtocol),nil); {Rescheduled by Timer Event}

      {Create Transport Timer}
      SocketsTransportTimer:=TimerCreateEx(SOCKETS_TRANSPORT_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsProcessTransport),nil); {Rescheduled by Timer Event}

      {Create Adapter Timer}
      SocketsAdapterTimer:=TimerCreateEx(SOCKETS_ADAPTER_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsProcessAdapter),nil); {Rescheduled by Timer Event}
      
      {Register Notification}
      NetworkDeviceNotification(nil,SocketsNetworkDeviceNotify,nil,DEVICE_NOTIFICATION_REGISTER or DEVICE_NOTIFICATION_DEREGISTER or DEVICE_NOTIFICATION_CLOSING,NOTIFIER_FLAG_NONE);
      
      {Register Shutdown}
      //To Do
      
      {$IFDEF SOCKET_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Start completed');
      {$ENDIF}
 
      {Return Result} 
      Result:=ERROR_SUCCESS;
      SocketsStartupError:=ERROR_SUCCESS;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SocketsLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;
 
{==============================================================================}

function SocketsStop:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Clients}
 if DNSClient = nil then Exit;
 
 {Check Managers}
 if ProtocolManager = nil then Exit;
 if TransportManager = nil then Exit;
 if AdapterManager = nil then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SocketsLock) = ERROR_SUCCESS then
  begin
   try
    {Check Started}
    Result:=ERROR_OPERATION_FAILED;
    if SocketsStartupCount = 0 then Exit;
    
    {Decrement Count}
    Dec(SocketsStartupCount);
    Result:=ERROR_SUCCESS;
    if SocketsStartupCount > 0 then Exit;
    
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Final stop call');
    {$ENDIF}
    
    {Shutdown and Cleanup}
    Result:=ERROR_OPERATION_FAILED;
    
    {Deregister Shutdown}
    //To Do
    
    {Deregister Notification}
    NetworkDeviceNotification(nil,SocketsNetworkDeviceNotify,nil,DEVICE_NOTIFICATION_NONE,NOTIFIER_FLAG_NONE);

    {Destroy Adapter Timer}
    TimerDestroy(SocketsAdapterTimer);
    
    {Destroy Transport Timer}
    TimerDestroy(SocketsTransportTimer);

    {Destroy Protocol Timer}
    TimerDestroy(SocketsProtocolTimer);

    {Destroy Filter Timer}
    TimerDestroy(SocketsFilterTimer);

    {Destroy Config Timer}
    TimerDestroy(SocketsConfigTimer);
    
    //To Do //Unbind Clients ? //No ?
    
    //To Do //Unbind Protocols ? //No ?
    
    {Unbind Transports} //To Do //Move to TransportUnbind ?
    if not TransportManager.UnbindTransports(nil) then
     begin
      if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to unbind one or more network transports');
     end;
    
    {Stop Clients}
    if DNSStop <> ERROR_SUCCESS then Exit;
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Stopped clients');
    {$ENDIF}

    {Stop Protocols}
    if ProtocolStop <> ERROR_SUCCESS then Exit;
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Stopped protocols');
    {$ENDIF}
   
    {Stop Transports}
    if TransportStop <> ERROR_SUCCESS then Exit;
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Stopped transports');
    {$ENDIF}
  
    {Stop Adapters}
    if NetworkStop <> ERROR_SUCCESS then Exit;
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Stopped adapters');
    {$ENDIF}
  
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Stop completed');
    {$ENDIF}

    {Return Result} 
    Result:=ERROR_SUCCESS;
    SocketsStartupError:=ERROR_NOT_READY;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SocketsLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;
 
{==============================================================================}
 
procedure SocketsAsyncStart(Data:Pointer);
begin
 {}
 {Wait for Ready}
 while not(SysInitCompleted) do
  begin
   ThreadSleep(0);
  end;
 
 {Start Sockets}
 SocketsStart;
end;

{==============================================================================}
{==============================================================================}
{Sockets Functions}
function SocketError: cint;
begin
 {}
 Result:=GetLastError;
end;

{==============================================================================}

function Socket(Domain,SocketType,Protocol:Longint):Longint;
begin
 {}
 Result:=fpsocket(Domain,SocketType,Protocol);
end;

{==============================================================================}

function Send(Sock:Longint;const Buf;BufLen,Flags:Longint):Longint;
begin
 {}
 Result:=fpsend(sock,@buf,buflen,flags);
end;

{==============================================================================}

function SendTo(Sock:Longint;const Buf;BufLen,Flags:Longint;var Addr; AddrLen : Longint):Longint;
begin
 {}
 Result:=fpsendto(sock,@buf,buflen,flags,@addr,addrlen);
end;

{==============================================================================}

function Recv(Sock:Longint;var Buf;BufLen,Flags:Longint):Longint;
begin
 {}
 Result:=fpRecv(Sock,@Buf,BufLen,Flags);
end;

{==============================================================================}

function RecvFrom(Sock : Longint; var Buf; Buflen,Flags : Longint; var Addr; var AddrLen : longint) : longint;
begin
 {}
 Result:=fpRecvFrom(Sock,@Buf,BufLen,Flags,@Addr,@AddrLen);
end;

{==============================================================================}

function Connect(Sock:Longint;const Addr;Addrlen:Longint):Boolean;
begin
 {}
 Result:=(fpconnect(sock,@addr,addrlen) = ERROR_SUCCESS);
end;

{==============================================================================}

function Shutdown(Sock:Longint;How:Longint):Longint;
begin
 {}
 Result:=fpshutdown(sock,how);
end;

{==============================================================================}

function Bind(Sock:Longint;const Addr;AddrLen:Longint):Boolean;
begin
 {}
 Result:=(fpBind(Sock,@Addr,AddrLen) = ERROR_SUCCESS);
end;

{==============================================================================}

function Listen(Sock,MaxConnect:Longint):Boolean;
begin
 {}
 Result:=(fplisten(Sock,MaxConnect) = ERROR_SUCCESS);
end;

{==============================================================================}

function Accept(Sock:Longint;var Addr;var Addrlen:Longint):Longint;
begin
 {}
 Result:=fpaccept(sock,@addr,@addrlen);
end;

{==============================================================================}

function GetSocketName(Sock:Longint;var Addr;var Addrlen:Longint):Longint;
begin
 {}
 Result:=fpGetSockName(sock,@addr,@addrlen);
end;

{==============================================================================}

function GetPeerName(Sock:Longint;var Addr;var Addrlen:Longint):Longint;
begin
 {}
 Result:=fpGetPeerName(Sock,@addr,@addrlen);
end;

{==============================================================================}

function GetSocketOptions(Sock,Level,OptName:Longint;var OptVal;var optlen:longint):Longint;
begin
 {}
 Result:=fpGetSockOpt(sock,level,optname,@optval,@optlen);
end;

{==============================================================================}

function SetSocketOptions(Sock,Level,OptName:Longint;const OptVal;optlen:longint):Longint;
begin
 {}
 Result:=fpsetsockopt(sock,level,optname,@optval,optlen);
end;

{==============================================================================}

function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;
begin
 {}
 Result:=fpsocketpair(domain,sockettype,protocol,@pair[1]);
end;

{==============================================================================}

function CloseSocket(Sock:Longint):Longint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(Sock);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(Sock,True,NETWORK_LOCK_READ) then Exit;

  {Close Socket}
  Result:=Socket.Protocol.CloseSocket(Socket);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: CloseSocket ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function Inet_Addr(cp: PChar): Longint;
begin
 {}
 Result:=Longint(StringToInAddr(cp));
end;

{==============================================================================}

function Inet_Ntoa(inaddr: TInAddr): PChar; 
{As per the Winsock specification, the buffer returned by this function is only
 guaranteed to be valid until the next Sockets function call is made within the
 same thread. Therefore, the data should be copied before another Sockets call}
var
 WorkBuffer:String;
 NetToAddr:PNetToAddr;
begin
 {}
 Result:=nil;
 
 {Get TLS Value}
 NetToAddr:=ThreadGetTlsValue(SocketsTlsIndex);
 if NetToAddr = nil then
  begin
   {Allocate TLS Value}
   NetToAddr:=AllocMem(SocketsTlsSize);
   if NetToAddr = nil then Exit;
   
   {Set TLS Value}
   if ThreadSetTlsValue(SocketsTlsIndex,NetToAddr) <> ERROR_SUCCESS then Exit;
  end;

 {Convert to String}  
 WorkBuffer:=InAddrToString(inaddr);
 if WorkBuffer <> '' then
  begin
   {Copy to Buffer}
   StrLCopy(PChar(NetToAddr),PChar(WorkBuffer),MAX_NAME_SIZE);
   Result:=PChar(NetToAddr); 
  end;
end;

{==============================================================================}

function Inet_Pton(family: Longint; Source: PChar; Dest: Pointer): Longint;
begin
 {}
 Result:=SOCKET_ERROR;
 SetLastError(WSAEFAULT);
 
 {Check Source}
 if Source = nil then Exit;
 
 {Check Dest}
 if Dest = nil then Exit;
 
 {Check Family}
 SetLastError(WSAEAFNOSUPPORT);
 case Family of
  AF_INET:begin
    PInAddr(Dest)^:=StringToInAddr(Source);
    
    //To Do //Check result, if not valid return 0
    
    Result:=1; {As per Spec}
    SetLastError(ERROR_SUCCESS);
   end;
  AF_INET6:begin
    PIn6Addr(Dest)^:=StringToIn6Addr(Source);
    
    //To Do //Check result, if not valid return 0
    
    Result:=1; {As per Spec}
    SetLastError(ERROR_SUCCESS);
   end;  
 end;
end;

{==============================================================================}

function Inet_Ntop(family: Longint; Source: Pointer; Dest: PChar; Size: Longint): PChar;
var
 WorkBuffer:String;
begin
 {}
 Result:=nil;
 SetLastError(WSA_INVALID_PARAMETER);
 
 {Check Source}
 if Source = nil then Exit;
 
 {Check Dest}
 if Dest = nil then Exit;
  
 {Check Family}
 SetLastError(WSAEAFNOSUPPORT);
 case Family of
  AF_INET:begin
    SetLastError(WSA_INVALID_PARAMETER);
    if Size < INET_ADDRSTRLEN then Exit;
    
    WorkBuffer:=InAddrToString(PInAddr(Source)^);
    StrLCopy(Dest,PChar(WorkBuffer),Size);
    
    Result:=Dest;
   end;
  AF_INET6:begin
    SetLastError(WSA_INVALID_PARAMETER);
    if Size < INET6_ADDRSTRLEN then Exit;
    
    WorkBuffer:=In6AddrToString(PIn6Addr(Source)^);
    StrLCopy(Dest,PChar(WorkBuffer),Size);
    
    Result:=Dest;
   end;  
 end;
end;

{==============================================================================}

function GetHostByAddr(addr: Pointer; len, family: Longint): PHostEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Client}
  SetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;
  
  {Get Host By Address}
  Result:=DNSClient.GetHostByAddr(addr,len,family);
 except
  on E: Exception do
   begin
    Result:=nil;
    SetLastError(WSANO_RECOVERY);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: GetHostByAddr ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function GetHostByName(name: PChar): PHostEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  SetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Host By Name}
  Result:=DNSClient.GetHostByName(name);
 except
  on E: Exception do
   begin
    Result:=nil;
    SetLastError(WSANO_RECOVERY);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: GetHostByName ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function GetHostName(name: PChar; len: Longint): Longint; 
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  SetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Host Name}
  Result:=DNSClient.GetHostName(name,len);
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    SetLastError(WSANO_RECOVERY);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: GetHostName ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function GetNetByAddr(addr: Pointer; len, Struct: Integer): PNetEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  SetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Network By Address}
  Result:=DNSClient.GetNetByAddr(addr,len,Struct);
 except
  on E: Exception do
   begin
    Result:=nil;
    SetLastError(WSANO_RECOVERY);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: GetNetByAddr ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function GetNetByName(name: PChar): PNetEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  SetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Network By Name}
  Result:=DNSClient.GetNetByName(name);
 except
  on E: Exception do
   begin
    Result:=nil;
    SetLastError(WSANO_RECOVERY);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: GetNetByName ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function GetServByPort(port: Longint; proto: PChar): PServEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  SetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Service By Port}
  Result:=DNSClient.GetServByPort(port,proto);
 except
  on E: Exception do
   begin
    Result:=nil;
    SetLastError(WSANO_RECOVERY);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: GetServByPort ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function GetServByName(name, proto: PChar): PServEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  SetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Service By Name}
  Result:=DNSClient.GetServByName(name,proto);
 except
  on E: Exception do
   begin
    Result:=nil;
    SetLastError(WSANO_RECOVERY);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: GetServByName ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function GetProtoByNumber(proto: Longint): PProtoEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  SetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Protocol By Number}
  Result:=DNSClient.GetProtoByNumber(proto);
 except
  on E: Exception do
   begin
    Result:=nil;
    SetLastError(WSANO_RECOVERY);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: GetProtoByNumber ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function GetProtoByName(name: PChar): PProtoEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  SetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Protocol By Name}
  Result:=DNSClient.GetProtoByName(name);
 except
  on E: Exception do
   begin
    Result:=nil;
    SetLastError(WSANO_RECOVERY);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: GetProtoByName ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function GetAddrInfo(HostName, ServName: PChar; Hints: PAddrInfo; var Addr: PAddrInfo): Longint;
begin
 {}
 Result:=WSAEAFNOSUPPORT;
 try
  {Set Result}
  Addr:=nil;
  
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  SetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;
  
  //To Do //See: DNSClient.GetAddrInfo
 except
  on E: Exception do
   begin
    Result:=WSANO_RECOVERY;
    SetLastError(WSANO_RECOVERY);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: GetAddrInfo ' + E.Message);
    {$ENDIF}
   end;
 end;
end;
  
{==============================================================================}

procedure FreeAddrInfo(ai: PAddrInfo);
begin
 {}
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  SetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;
  
  //To Do //See: DNSClient.FreeAddrInfo
 except
  on E: Exception do
   begin
    SetLastError(WSANO_RECOVERY);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: FreeAddrInfo ' + E.Message);
    {$ENDIF}
   end;
 end;
end;
  
{==============================================================================}

function GetNameInfo(sa: PSockAddr; salen: Longint; host: PChar; hostlen: DWORD; serv: PChar; servlen: DWORD; flags: Longint): Longint;
begin
 {}
 Result:=WSAEAFNOSUPPORT;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
 
  {Check Client}
  SetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  //To Do //See: DNSClient.GetNameInfo
 except
  on E: Exception do
   begin
    Result:=WSANO_RECOVERY;
    SetLastError(WSANO_RECOVERY);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: GetNameInfo ' + E.Message);
    {$ENDIF}
   end;
 end;
end;
  
{==============================================================================}
{==============================================================================}
{RTL Sockets Functions}
function fpsocket(domain:cint; xtype:cint; protocol: cint):cint;
begin
 {}
 Result:=LongInt(INVALID_SOCKET);
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Manager}
  SetLastError(WSASYSNOTREADY);
  if ProtocolManager = nil then Exit;
  
  {Create Socket}
  Result:=ProtocolManager.Socket(domain,xtype,protocol);
 except
  on E: Exception do
   begin
    Result:=LongInt(INVALID_SOCKET);
    SetLastError(WSAEPROTONOSUPPORT);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpsocket ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpsend(s:cint; msg:pointer; len:size_t; flags:cint):ssize_t;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=Word(SOCKET_ERROR);
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Send Socket}
  Result:=Socket.Protocol.Send(Socket,msg^,len,flags);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=Word(SOCKET_ERROR);
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpsend ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpsendto(s:cint; msg:pointer; len:size_t; flags:cint; tox :psockaddr; tolen: tsocklen):ssize_t;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=Word(SOCKET_ERROR);
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Send To Socket}
  Result:=Socket.Protocol.SendTo(Socket,msg^,len,flags,tox^,tolen);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=Word(SOCKET_ERROR);
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpsendto ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fprecv(s:cint; buf: pointer; len: size_t; flags: cint):ssize_t;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=Word(SOCKET_ERROR);
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Receive Socket}
  Result:=Socket.Protocol.Recv(Socket,buf^,len,flags);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=Word(SOCKET_ERROR);
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fprecv ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fprecvfrom(s:cint; buf: pointer; len: size_t; flags: cint; from : psockaddr; fromlen : psocklen):ssize_t;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=Word(SOCKET_ERROR);
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Receive From Socket}
  Result:=Socket.Protocol.RecvFrom(Socket,buf^,len,flags,from^,fromlen^);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=Word(SOCKET_ERROR);
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fprecvfrom ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpconnect(s:cint; name  : psockaddr; namelen : tsocklen):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Connect Socket}
  Result:=Socket.Protocol.Connect(Socket,name^,namelen);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpconnect ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpshutdown(s:cint; how:cint):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;
  
  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Shutdown Socket}
  Result:=Socket.Protocol.Shutdown(Socket,How);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpshutdown ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpbind(s:cint; addrx : psockaddr; addrlen : tsocklen):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Bind Socket}
  Result:=Socket.Protocol.Bind(Socket,addrx^,addrlen);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpbind ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fplisten(s:cint; backlog : cint):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Listen Socket}
  Result:=Socket.Protocol.Listen(Socket,backlog);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fplisten ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpaccept(s:cint; addrx : psockaddr; addrlen : psocklen):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=LongInt(INVALID_SOCKET);
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;
  
  {Check Manager}
  if ProtocolManager = nil then Exit;
  
  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Accept Socket}
  Result:=TSocket(Socket.Protocol.Accept(Socket,addrx,addrlen));
  
  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=LongInt(INVALID_SOCKET);
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpaccept ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpgetsockname(s:cint; name  : psockaddr; namelen : psocklen):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Get Socket Name}
  Result:=Socket.Protocol.GetSockName(Socket,name^,namelen^);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpgetsockname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpgetpeername(s:cint; name  : psockaddr; namelen : psocklen):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Get Socket Peer Name}
  Result:=Socket.Protocol.GetPeerName(Socket,name^,namelen^);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpgetpeername ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpgetsockopt(s:cint; level:cint; optname:cint; optval:pointer; optlen : psocklen):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Get Socket Options}
  Result:=Socket.Protocol.GetSockOpt(Socket,level,optname,optval,optlen^);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpgetsockopt ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpsetsockopt(s:cint; level:cint; optname:cint; optval:pointer; optlen :tsocklen):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  SetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  SetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Set Socket Options}
  Result:=Socket.Protocol.SetSockOpt(Socket,level,optname,optval,optlen);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    SetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpsetsockopt ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpsocketpair(d:cint; xtype:cint; protocol:cint; sv:pcint):cint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 SetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}
{==============================================================================}
{Sockets Helper Functions}
procedure SocketsProcessConfig(Data:Pointer);
begin
 {}
 try
  {$IFDEF SOCKET_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Process configs');
  {$ENDIF}
 
  {Check Manager}
  if ProtocolManager = nil then Exit;
  
  {Process Configs}
  ProtocolManager.ProcessConfigs;
  
 finally
  {Enable Timer}
  TimerEnable(SocketsConfigTimer);
 end;
end;

{==============================================================================}

procedure SocketsProcessFilter(Data:Pointer);
begin
 {}
 try
  {$IFDEF SOCKET_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Process filters');
  {$ENDIF}
 
  {Check Manager}
  if ProtocolManager = nil then Exit;
  
  {Process Filters}
  ProtocolManager.ProcessFilters;
 
 finally 
  {Enable Timer}
  TimerEnable(SocketsFilterTimer);
 end; 
end;

{==============================================================================}

procedure SocketsProcessProtocol(Data:Pointer);
begin
 {}
 try
  {$IFDEF SOCKET_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Process protocols');
  {$ENDIF}
 
  {Check Manager}
  if ProtocolManager = nil then Exit;
  
  {Process Protocols}
  ProtocolManager.ProcessProtocols;
  
 finally
  {Enable Timer}
  TimerEnable(SocketsProtocolTimer);
 end; 
end;

{==============================================================================}

procedure SocketsProcessTransport(Data:Pointer);
begin
 {}
 try
  {$IFDEF SOCKET_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Process transports');
  {$ENDIF}
 
  {Check Manager}
  if TransportManager = nil then Exit;
  
  {Process Transports}
  TransportManager.ProcessTransports;
  
 finally
  {Enable Timer}
  TimerEnable(SocketsTransportTimer);
 end; 
end;

{==============================================================================}

procedure SocketsProcessAdapter(Data:Pointer);
begin
 {}
 try
  {$IFDEF SOCKET_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Process adapters');
  {$ENDIF}
 
  {Check Manager}
  if AdapterManager = nil then Exit;
  
  {Process Adapters}
  AdapterManager.ProcessStatus;
  
 finally  
  {Enable Timer}
  TimerEnable(SocketsAdapterTimer);
 end; 
end;

{==============================================================================}

procedure SocketsNetworkDeviceAdd(Event:PSocketsDeviceEvent);
var
 Adapter:TNetworkAdapter;
begin
 {}
 {$IFDEF SOCKET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Network device add');
 {$ENDIF}
 
 {Check Event}
 if Event = nil then Exit;
 if Event.Device = nil then Exit;
 
 {$IFDEF SOCKET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets:  Device = ' + DeviceGetName(@Event.Device.Device));
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
 if ProtocolManager = nil then Exit;
  
 {Acquire the Lock}
 if CriticalSectionLock(SocketsLock) = ERROR_SUCCESS then
  begin
   try
    {Check Started}
    if SocketsStartupCount > 0 then
     begin
      {Check Type}
      case Event.Device.Device.DeviceType of
       NETWORK_TYPE_ETHERNET,NETWORK_TYPE_TOKENRING:begin
         {Check Adapter}
         Adapter:=AdapterManager.GetAdapterByDevice(Event.Device,False,NETWORK_LOCK_NONE); {Do not lock}
         if Adapter = nil then
          begin
           {Create Adapter}
           if WIRED_NETWORK_ENABLED then
            begin
             Adapter:=TWiredAdapter.Create(AdapterManager,Event.Device,DeviceGetName(@Event.Device.Device));
         
             {Start Adapter}
             Adapter.StartAdapter;
         
             {Bind Transports}
             TransportManager.BindTransports(Adapter);
            end; 
          end; 
        end;
      end; 
     end;  
   finally
    {Release the Lock}
    CriticalSectionUnlock(SocketsLock);
   end;
  end;
  
 {Destroy Event}
 FreeMem(Event); 
end;

{==============================================================================}

function SocketsNetworkDeviceRemove(Network:PNetworkDevice):LongWord;
var
 Adapter:TNetworkAdapter;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF SOCKET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Network device remove');
 {$ENDIF}
 
 {Check Network}
 if Network = nil then Exit;

 {$IFDEF SOCKET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets:  Device = ' + DeviceGetName(@Network.Device));
 {$ENDIF}
 
 {Check Managers}
 if AdapterManager = nil then Exit;
 if TransportManager = nil then Exit;
 if ProtocolManager = nil then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(SocketsLock) = ERROR_SUCCESS then
  begin
   try
    {Check Started}
    if SocketsStartupCount > 0 then
     begin
      {Check Type}
      case Network.Device.DeviceType of
       NETWORK_TYPE_ETHERNET,NETWORK_TYPE_TOKENRING:begin
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
   finally
    {Release the Lock}
    CriticalSectionUnlock(SocketsLock);
   end;
  end;
end;

{==============================================================================}

function SocketsNetworkDeviceEnum(Network:PNetworkDevice;Data:Pointer):LongWord;
var
 Adapter:TNetworkAdapter;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {$IFDEF SOCKET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Network device enumeration');
 {$ENDIF}

 {Check Network}
 if Network = nil then Exit;
 
 {$IFDEF SOCKET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets:  Device = ' + DeviceGetName(@Network.Device));
 {$ENDIF}
 
 {Check Manager}
 if AdapterManager = nil then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(SocketsLock) = ERROR_SUCCESS then
  begin
   try
    {Check Started}
    if SocketsStartupCount > 0 then
     begin
      {Check Type}
      case Network.Device.DeviceType of
       NETWORK_TYPE_ETHERNET,NETWORK_TYPE_TOKENRING:begin
         {Check Adapter}
         Adapter:=AdapterManager.GetAdapterByDevice(Network,False,NETWORK_LOCK_NONE); {Do not lock}
         if Adapter = nil then
          begin
           {Create Adapter}
           if WIRED_NETWORK_ENABLED then
            begin
             TWiredAdapter.Create(AdapterManager,Network,DeviceGetName(@Network.Device));
            end; 
          end; 
        end;
      end; 
     end;  
   finally
    {Release the Lock}
    CriticalSectionUnlock(SocketsLock);
   end;
  end;
end;

{==============================================================================}

function SocketsNetworkDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
var
 Adapter:TNetworkAdapter;
 Event:PSocketsDeviceEvent;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {$IFDEF SOCKET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Network device notification (Notification=' + NotificationToString(Notification) + ')');
 {$ENDIF}
 
 {Check Device}
 if Device = nil then Exit;
 
 {Check Manager}
 if AdapterManager = nil then Exit;
 
 {Check Notification}
 if (Notification and DEVICE_NOTIFICATION_REGISTER) <> 0 then
  begin
   {Acquire the Lock}
   if CriticalSectionLock(SocketsLock) = ERROR_SUCCESS then
    begin
     try
      {Check Started}
      if SocketsStartupCount > 0 then
       begin
        {Check Adapter}
        Adapter:=AdapterManager.GetAdapterByDevice(PNetworkDevice(Device),False,NETWORK_LOCK_NONE); {Do not lock}
        if Adapter = nil then
         begin
          {Create Event}
          Event:=AllocMem(SizeOf(TSocketsDeviceEvent));
          if Event = nil then Exit;
          
          {Setup Event}
          Event.Timer:=INVALID_HANDLE_VALUE;
          Event.Device:=PNetworkDevice(Device);
          
          {Create Timer}
          Event.Timer:=TimerCreateEx(SOCKETS_DEVICE_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsNetworkDeviceAdd),Event);
          if Event.Timer = INVALID_HANDLE_VALUE then
           begin
            {Destroy Event}
            FreeMem(Event);
           end;
         end;
       end;
     finally
      {Release the Lock}
      CriticalSectionUnlock(SocketsLock);
     end;
    end;
  end
 else if (Notification and DEVICE_NOTIFICATION_CLOSING) <> 0 then
  begin
   {Acquire the Lock}
   if CriticalSectionLock(SocketsLock) = ERROR_SUCCESS then
    begin
     try
      {Check Started}
      if SocketsStartupCount > 0 then
       begin
        {Check Adapter}
        Adapter:=AdapterManager.GetAdapterByDevice(PNetworkDevice(Device),False,NETWORK_LOCK_NONE); {Do not lock}
        if Adapter <> nil then
         begin
          {Remove Adapter}
          SocketsNetworkDeviceRemove(PNetworkDevice(Device));
         end;
       end;
     finally
      {Release the Lock}
      CriticalSectionUnlock(SocketsLock);
     end;
    end;
  end
 else if (Notification and DEVICE_NOTIFICATION_DEREGISTER) <> 0 then
  begin
   {Acquire the Lock}
   if CriticalSectionLock(SocketsLock) = ERROR_SUCCESS then
    begin
     try
      {Check Started}
      if SocketsStartupCount > 0 then
       begin
        {Check Adapter}
        Adapter:=AdapterManager.GetAdapterByDevice(PNetworkDevice(Device),False,NETWORK_LOCK_NONE); {Do not lock}
        if Adapter <> nil then
         begin
          {Remove Adapter}
          SocketsNetworkDeviceRemove(PNetworkDevice(Device));
         end;
       end;
     finally
      {Release the Lock}
      CriticalSectionUnlock(SocketsLock);
     end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}

initialization
 SocketsInit;
 if SOCKETS_AUTOSTART then
  begin
   if not SOCKETS_ASYNCSTART then
    begin
     {Start Sockets}
     SocketsStart;
    end
   else
    begin
     {Schedule Worker}
     WorkerSchedule(250,TWorkerTask(SocketsAsyncStart),nil,nil); {Delay start to allow device initialization}
    end;
  end; 
 
{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end. 