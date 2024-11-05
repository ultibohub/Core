{
Ultibo Network Sockets interface unit.

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

 
Network Sockets
===============

 This unit incorporates the socketsh.inc header from rtl-extra and
 adds Ultibo specific functionality. The unit can then be used in 
 place of the sockets unit normally provided by the rtl-extra package.
 
 Notes: All BSD functions that accept an Address or Port expect
        them to be in Network order. All other functions that take
        an Address or Port expect them to be in Host order
        
        This unit includes the interface normally provided by the sockets unit
        in the FPC package rtl-extras. The rtl-extras package does not build
        the sockets unit for target Ultibo as this unit will always be available
        to provide the required functionality for any other package that uses
        it.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Sockets;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,Platform,Threads,Devices,SysUtils,Classes,Network,Transport,Protocol,
     Loopback,ARP,IP,IPv6,UDP,TCP,ICMP,ICMPv6,IGMP,RAW,DHCP,DNS,CTypes;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
  
{==============================================================================}
const
 {Sockets specific constants}
 SOCKETS_DEVICE_TIMER_INTERVAL    = 100; {Timer interval for new device additions}

 SOCKETS_CONFIG_TIMER_INTERVAL    = 1000;
 SOCKETS_FILTER_TIMER_INTERVAL    = 1000;
 SOCKETS_PROTOCOL_TIMER_INTERVAL  = 250;  {Previously 100}
 SOCKETS_SOCKET_TIMER_INTERVAL    = 1000;
 SOCKETS_AUTH_TIMER_INTERVAL      = 1000;
 SOCKETS_MONITOR_TIMER_INTERVAL   = 1000;
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
 EsockADDRINUSE        = WSAEADDRINUSE;
 
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
 SO_REUSEPORT    = GlobalSock.SO_REUSEPORT;
 SO_TIMESTAMP    = GlobalSock.SO_TIMESTAMP;
 SO_ACCEPTFILTER = GlobalSock.SO_ACCEPTFILTER;

 SO_DONTLINGER       = GlobalSock.SO_DONTLINGER;
 SO_EXCLUSIVEADDRUSE = GlobalSock.SO_EXCLUSIVEADDRUSE;

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

 MSG_INTERRUPT   = GlobalSock.MSG_INTERRUPT;
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
  
const
 NoAddress : in_addr  = (s_addr:0);
 NoNet     : in_addr  = (s_addr:0);
 NoAddress6: in6_addr = (u6_addr16:(0,0,0,0,0,0,0,0));
 NoNet6    : in6_addr = (u6_addr16:(0,0,0,0,0,0,0,0));
  
const
 {FD set sizes for select}
 FD_MAXFDSET = GlobalSock.FD_SETSIZE;
  
{==============================================================================}
type
 {Sockets specific types}
 PSocketsDeviceEvent = ^TSocketsDeviceEvent;
 TSocketsDeviceEvent = record
  Timer:TTimerHandle;
  Device:PNetworkDevice;
 end;

type
 size_t = GlobalTypes.SIZE_T;   {cuint32;}
 ssize_t = GlobalTypes.SSIZE_T; {cint32;}
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
 TSockPairArray = array[0..1] of clong;
 TSockArray  = array[1..2] of clong;              {Legacy}

 psockaddr_un = ^sockaddr_un;
 sockaddr_un = packed record
  sun_family:sa_family_t;
  sun_path:array[0..107] of char;
 end;

 {Tsocket = clong;}  {To ease porting code from Kylix libc unit to sockets unit.}

type
 {FD set type for select}
 PFDSet = GlobalSock.PFDSet;
 TFDSet = GlobalSock.TFDSet;
 
 {TimeVal type for select}
 TTimeVal = GlobalSock.TTimeVal;
 PTimeVal = GlobalSock.PTimeVal;
 
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

function Socket(Domain,SocketType,Protocol:Longint):clong;
function Send(Sock:clong;const Buf;BufLen,Flags:Longint):Longint;
function SendTo(Sock:clong;const Buf;BufLen,Flags:Longint;var Addr; AddrLen : Longint):Longint;
function Recv(Sock:clong;var Buf;BufLen,Flags:Longint):Longint;
function RecvFrom(Sock : clong; var Buf; Buflen,Flags : Longint; var Addr; var AddrLen : longint) : longint;
function Connect(Sock:clong;const Addr;Addrlen:Longint):Boolean; overload;
function Shutdown(Sock:clong;How:Longint):Longint;
function Bind(Sock:clong;const Addr;AddrLen:Longint):Boolean;
function Listen(Sock:clong;MaxConnect:Longint):Boolean;
function Accept(Sock:clong;var Addr;var Addrlen:Longint):clong; overload;
function GetSocketName(Sock:clong;var Addr;var Addrlen:Longint):Longint;
function GetPeerName(Sock:clong;var Addr;var Addrlen:Longint):Longint;
function GetSocketOptions(Sock:clong;Level,OptName:Longint;var OptVal;var optlen:longint):Longint;
function SetSocketOptions(Sock:clong;Level,OptName:Longint;const OptVal;optlen:longint):Longint;
function SocketPair(Domain,SocketType,Protocol:Longint;var Pair:TSockArray):Longint;

function CloseSocket(Sock:clong):Longint;

function Inet_Addr(cp: PChar): Longint;
function Inet_Ntoa(inaddr: TInAddr): PChar; 
function Inet_Aton(cp: PChar; inaddr: PInAddr): Longint;

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
function fpsocket(domain:cint; xtype:cint; protocol: cint):clong;
function fpsend(s:clong; msg:pointer; len:size_t; flags:cint):ssize_t;
function fpsendto(s:clong; msg:pointer; len:size_t; flags:cint; tox :psockaddr; tolen: tsocklen):ssize_t;
function fprecv(s:clong; buf: pointer; len: size_t; flags: cint):ssize_t;
function fprecvfrom(s:clong; buf: pointer; len: size_t; flags: cint; from : psockaddr; fromlen : psocklen):ssize_t;
function fpconnect(s:clong; name  : psockaddr; namelen : tsocklen):cint;
function fpshutdown(s:clong; how:cint):cint;
function fpbind(s:clong; addrx : psockaddr; addrlen : tsocklen):cint;
function fplisten(s:clong; backlog : cint):cint;
function fpaccept(s:clong; addrx : psockaddr; addrlen : psocklen):clong;
function fpgetsockname(s:clong; name  : psockaddr; namelen : psocklen):cint;
function fpgetpeername(s:clong; name  : psockaddr; namelen : psocklen):cint;
function fpgetsockopt(s:clong; level:cint; optname:cint; optval:pointer; optlen : psocklen):cint;
function fpsetsockopt(s:clong; level:cint; optname:cint; optval:pointer; optlen :tsocklen):cint;
function fpsocketpair(d:cint; xtype:cint; protocol:cint; sv:pclong):cint;

{==============================================================================}
{RTL Select Function (Sockets only)}
function fpselect(n:cint; readfds, writefds, exceptfds: PFDSet; TimeOut: PTimeVal):cint;

function fpFD_SET(fdno:clong; var nset: TFDSet):cint;
function fpFD_CLR(fdno:clong; var nset: TFDSet):cint;
function fpFD_ZERO(out nset: TFDSet):cint;
function fpFD_ISSET(fdno:clong; const nset: TFDSet): cint;

{==============================================================================}
{RTL File/Text Sockets Functions}
procedure Sock2Text(Sock:clong;Var SockIn,SockOut:Text); deprecated;
function Accept(Sock:clong;var addr:TInetSockAddr;var SockIn,SockOut:File):Boolean; deprecated; overload;
function Accept(Sock:clong;var addr:TInetSockAddr;var SockIn,SockOut:text):Boolean; deprecated; overload;
function Connect(Sock:clong;const addr:TInetSockAddr;var SockIn,SockOut:text):Boolean; deprecated; overload;
function Connect(Sock:clong;const addr:TInetSockAddr;var SockIn,SockOut:file):Boolean; deprecated; overload;
procedure Sock2File(Sock:clong;Var SockIn,SockOut:File); deprecated;

{==============================================================================}
{Sockets Helper Functions}
procedure SocketsProcessConfig(Data:Pointer);
procedure SocketsProcessFilter(Data:Pointer);
procedure SocketsProcessProtocol(Data:Pointer);
procedure SocketsProcessSocket(Data:Pointer);
procedure SocketsProcessAuth(Data:Pointer);
procedure SocketsProcessMonitor(Data:Pointer);
procedure SocketsProcessTransport(Data:Pointer);
procedure SocketsProcessAdapter(Data:Pointer);

procedure SocketsNetworkDeviceAdd(Event:PSocketsDeviceEvent);
function SocketsNetworkDeviceRemove(Network:PNetworkDevice):LongWord;
function SocketsNetworkDeviceUp(Network:PNetworkDevice):LongWord;
function SocketsNetworkDeviceDown(Network:PNetworkDevice):LongWord;

function SocketsNetworkDeviceEnum(Network:PNetworkDevice;Data:Pointer):LongWord;
function SocketsNetworkDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

{==============================================================================}
{RTL Sockets Helper Functions}
function htonl(host:cardinal):cardinal; inline; 
function ntohl(net:cardinal):cardinal; inline; 
function htons(host:word):word; inline;
function ntohs(net:word):word; inline;

function NetAddrToStr(Entry:in_addr):AnsiString;
function HostAddrToStr(Entry:in_addr):AnsiString;
function StrToHostAddr(IP:AnsiString):in_addr;
function StrToNetAddr(IP:AnsiString):in_addr;

{Netdb legacy compatibility}
function HostToNet(Host:in_addr):in_addr; overload; deprecated;
function NetToHost(Net:in_addr):in_addr; overload; deprecated;
function HostToNet(Host:Longint):Longint; overload; deprecated;
function NetToHost(Net:Longint):Longint; overload; deprecated;
function ShortHostToNet(Host:Word):Word; deprecated;
function ShortNetToHost(Net:Word):Word; deprecated;

function HostAddrToStr6(Entry:Tin6_addr):AnsiString;
function StrToHostAddr6(IP:String):Tin6_addr; 
function NetAddrToStr6(Entry:Tin6_addr):AnsiString;
function StrToNetAddr6(IP:AnsiString):TIn6_Addr;

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
 SocketsSocketTimer:TTimerHandle = INVALID_HANDLE_VALUE;
 SocketsAuthTimer:TTimerHandle = INVALID_HANDLE_VALUE;
 SocketsMonitorTimer:TTimerHandle = INVALID_HANDLE_VALUE;
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

 {Check Settings}
 if NetworkSettings = nil then Exit;
 
 {Check Clients}
 if NetworkSettings.GetBoolean('DNS_CLIENT_ENABLED') and (DNSClient = nil) then Exit;
 
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
 
      {Register Notification}
      NetworkDeviceNotification(nil,SocketsNetworkDeviceNotify,nil,DEVICE_NOTIFICATION_REGISTER or DEVICE_NOTIFICATION_DEREGISTER 
                                or DEVICE_NOTIFICATION_CLOSING or DEVICE_NOTIFICATION_UP or DEVICE_NOTIFICATION_DOWN,NOTIFIER_FLAG_NONE);

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
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Started transports and monitors');
      {$ENDIF}
       
      {Start Protocols}
      if ProtocolStart <> ERROR_SUCCESS then Exit;
      {$IFDEF SOCKET_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Started protocols, filters and configs');
      {$ENDIF}
       
      {Start Clients}
      if DNSStart <> ERROR_SUCCESS then Exit;
      {$IFDEF SOCKET_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Started clients');
      {$ENDIF}
       
      {Bind Transports}
      if TransportBind <> ERROR_SUCCESS then Exit;
      {$IFDEF SOCKET_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Bound transports and monitors');
      {$ENDIF}
      
      {Create Config Timer}
      SocketsConfigTimer:=TimerCreateEx(SOCKETS_CONFIG_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsProcessConfig),nil); {Rescheduled by Timer Event}

      {Create Filter Timer}
      SocketsFilterTimer:=TimerCreateEx(SOCKETS_FILTER_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsProcessFilter),nil); {Rescheduled by Timer Event}

      {Create Protocol Timer}
      SocketsProtocolTimer:=TimerCreateEx(SOCKETS_PROTOCOL_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsProcessProtocol),nil); {Rescheduled by Timer Event}
      
      {Create Socket Timer}
      SocketsSocketTimer:=TimerCreateEx(SOCKETS_SOCKET_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsProcessSocket),nil); {Rescheduled by Timer Event}

      {Create Auth Timer}
      SocketsAuthTimer:=TimerCreateEx(SOCKETS_AUTH_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsProcessAuth),nil); {Rescheduled by Timer Event}
      
      {Create Monitor Timer}
      SocketsMonitorTimer:=TimerCreateEx(SOCKETS_MONITOR_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsProcessMonitor),nil); {Rescheduled by Timer Event}
      
      {Create Transport Timer}
      SocketsTransportTimer:=TimerCreateEx(SOCKETS_TRANSPORT_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsProcessTransport),nil); {Rescheduled by Timer Event}

      {Create Adapter Timer}
      SocketsAdapterTimer:=TimerCreateEx(SOCKETS_ADAPTER_TIMER_INTERVAL,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(SocketsProcessAdapter),nil); {Rescheduled by Timer Event}
     
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
 
 {Check Managers}
 if ProtocolManager = nil then Exit;
 if TransportManager = nil then Exit;
 if AdapterManager = nil then Exit;

 {Check Settings}
 if NetworkSettings = nil then Exit;
 
 {Check Clients}
 if NetworkSettings.GetBoolean('DNS_CLIENT_ENABLED') and (DNSClient = nil) then Exit;
 
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
    
    {Deregister Notification}
    NetworkDeviceNotification(nil,SocketsNetworkDeviceNotify,nil,DEVICE_NOTIFICATION_NONE,NOTIFIER_FLAG_NONE);

    {Deregister Shutdown}
    //To Do

    {Destroy Adapter Timer}
    TimerDestroy(SocketsAdapterTimer);
    
    {Destroy Transport Timer}
    TimerDestroy(SocketsTransportTimer);

    {Destroy Monitor Timer}
    TimerDestroy(SocketsMonitorTimer);

    {Destroy Auth Timer}
    TimerDestroy(SocketsAuthTimer);
    
    {Destroy Socket Timer}
    TimerDestroy(SocketsSocketTimer);
    
    {Destroy Protocol Timer}
    TimerDestroy(SocketsProtocolTimer);

    {Destroy Filter Timer}
    TimerDestroy(SocketsFilterTimer);

    {Destroy Config Timer}
    TimerDestroy(SocketsConfigTimer);
    
    {Unbind Transports}
    if TransportUnbind <> ERROR_SUCCESS then Exit;
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Unbound transports and monitors');
    {$ENDIF}
    
    {Stop Clients}
    if DNSStop <> ERROR_SUCCESS then Exit;
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Stopped clients');
    {$ENDIF}

    {Stop Protocols}
    if ProtocolStop <> ERROR_SUCCESS then Exit;
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Stopped protocols, filters and configs');
    {$ENDIF}
   
    {Stop Transports}
    if TransportStop <> ERROR_SUCCESS then Exit;
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Stopped transports and monitors');
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
 Result:=NetworkGetLastError;
end;

{==============================================================================}

function Socket(Domain,SocketType,Protocol:Longint):clong;
begin
 {}
 Result:=fpsocket(Domain,SocketType,Protocol);
end;

{==============================================================================}

function Send(Sock:clong;const Buf;BufLen,Flags:Longint):Longint;
begin
 {}
 Result:=fpsend(sock,@buf,buflen,flags);
end;

{==============================================================================}

function SendTo(Sock:clong;const Buf;BufLen,Flags:Longint;var Addr; AddrLen : Longint):Longint;
begin
 {}
 Result:=fpsendto(sock,@buf,buflen,flags,@addr,addrlen);
end;

{==============================================================================}

function Recv(Sock:clong;var Buf;BufLen,Flags:Longint):Longint;
begin
 {}
 Result:=fpRecv(Sock,@Buf,BufLen,Flags);
end;

{==============================================================================}

function RecvFrom(Sock : clong; var Buf; Buflen,Flags : Longint; var Addr; var AddrLen : longint) : longint;
begin
 {}
 Result:=fpRecvFrom(Sock,@Buf,BufLen,Flags,@Addr,@AddrLen);
end;

{==============================================================================}

function Connect(Sock:clong;const Addr;Addrlen:Longint):Boolean;
begin
 {}
 Result:=(fpconnect(sock,@addr,addrlen) = ERROR_SUCCESS);
end;

{==============================================================================}

function Shutdown(Sock:clong;How:Longint):Longint;
begin
 {}
 Result:=fpshutdown(sock,how);
end;

{==============================================================================}

function Bind(Sock:clong;const Addr;AddrLen:Longint):Boolean;
begin
 {}
 Result:=(fpBind(Sock,@Addr,AddrLen) = ERROR_SUCCESS);
end;

{==============================================================================}

function Listen(Sock:clong;MaxConnect:Longint):Boolean;
begin
 {}
 Result:=(fplisten(Sock,MaxConnect) = ERROR_SUCCESS);
end;

{==============================================================================}

function Accept(Sock:clong;var Addr;var Addrlen:Longint):clong;
begin
 {}
 Result:=fpaccept(sock,@addr,@addrlen);
end;

{==============================================================================}

function GetSocketName(Sock:clong;var Addr;var Addrlen:Longint):Longint;
begin
 {}
 Result:=fpGetSockName(sock,@addr,@addrlen);
end;

{==============================================================================}

function GetPeerName(Sock:clong;var Addr;var Addrlen:Longint):Longint;
begin
 {}
 Result:=fpGetPeerName(Sock,@addr,@addrlen);
end;

{==============================================================================}

function GetSocketOptions(Sock:clong;Level,OptName:Longint;var OptVal;var optlen:longint):Longint;
begin
 {}
 Result:=fpGetSockOpt(sock,level,optname,@optval,@optlen);
end;

{==============================================================================}

function SetSocketOptions(Sock:clong;Level,OptName:Longint;const OptVal;optlen:longint):Longint;
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

function CloseSocket(Sock:clong):Longint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    NetworkSetLastError(WSAENOTSOCK);
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

function Inet_Aton(cp: PChar; inaddr: PInAddr): Longint;
begin
 {}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEFAULT);
 
 {Check Name}
 if cp = nil then Exit;
 
 {Check Address}
 if inaddr = nil then Exit;
 
 inaddr^:=StringToInAddr(cp);
 
 //To Do //Check result, if not valid return 0
    
 Result:=1; {As per Spec}
 NetworkSetLastError(ERROR_SUCCESS);
end; 

{==============================================================================}

function Inet_Pton(family: Longint; Source: PChar; Dest: Pointer): Longint;
begin
 {}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEFAULT);
 
 {Check Source}
 if Source = nil then Exit;
 
 {Check Dest}
 if Dest = nil then Exit;
 
 {Check Family}
 NetworkSetLastError(WSAEAFNOSUPPORT);
 case Family of
  AF_INET:begin
    PInAddr(Dest)^:=StringToInAddr(Source);
    
    //To Do //Check result, if not valid return 0
    
    Result:=1; {As per Spec}
    NetworkSetLastError(ERROR_SUCCESS);
   end;
  AF_INET6:begin
    PIn6Addr(Dest)^:=StringToIn6Addr(Source);
    
    //To Do //Check result, if not valid return 0
    
    Result:=1; {As per Spec}
    NetworkSetLastError(ERROR_SUCCESS);
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
 NetworkSetLastError(WSA_INVALID_PARAMETER);
 
 {Check Source}
 if Source = nil then Exit;
 
 {Check Dest}
 if Dest = nil then Exit;
  
 {Check Family}
 NetworkSetLastError(WSAEAFNOSUPPORT);
 case Family of
  AF_INET:begin
    NetworkSetLastError(WSA_INVALID_PARAMETER);
    if Size < INET_ADDRSTRLEN then Exit;
    
    WorkBuffer:=InAddrToString(PInAddr(Source)^);
    StrLCopy(Dest,PChar(WorkBuffer),Size);
    
    Result:=Dest;
   end;
  AF_INET6:begin
    NetworkSetLastError(WSA_INVALID_PARAMETER);
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
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;
  
  {Get Host By Address}
  Result:=DNSClient.GetHostByAddr(addr,len,family);
 except
  on E: Exception do
   begin
    Result:=nil;
    NetworkSetLastError(WSANO_RECOVERY);
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
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Host By Name}
  Result:=DNSClient.GetHostByName(name);
 except
  on E: Exception do
   begin
    Result:=nil;
    NetworkSetLastError(WSANO_RECOVERY);
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
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Host Name}
  Result:=DNSClient.GetHostName(name,len);
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSANO_RECOVERY);
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
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Network By Address}
  Result:=DNSClient.GetNetByAddr(addr,len,Struct);
 except
  on E: Exception do
   begin
    Result:=nil;
    NetworkSetLastError(WSANO_RECOVERY);
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
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Network By Name}
  Result:=DNSClient.GetNetByName(name);
 except
  on E: Exception do
   begin
    Result:=nil;
    NetworkSetLastError(WSANO_RECOVERY);
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
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Service By Port}
  Result:=DNSClient.GetServByPort(port,proto);
 except
  on E: Exception do
   begin
    Result:=nil;
    NetworkSetLastError(WSANO_RECOVERY);
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
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Service By Name}
  Result:=DNSClient.GetServByName(name,proto);
 except
  on E: Exception do
   begin
    Result:=nil;
    NetworkSetLastError(WSANO_RECOVERY);
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
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Protocol By Number}
  Result:=DNSClient.GetProtoByNumber(proto);
 except
  on E: Exception do
   begin
    Result:=nil;
    NetworkSetLastError(WSANO_RECOVERY);
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
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Protocol By Name}
  Result:=DNSClient.GetProtoByName(name);
 except
  on E: Exception do
   begin
    Result:=nil;
    NetworkSetLastError(WSANO_RECOVERY);
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
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;
  
  //To Do //See: DNSClient.GetAddrInfo
 except
  on E: Exception do
   begin
    Result:=WSANO_RECOVERY;
    NetworkSetLastError(WSANO_RECOVERY);
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
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;
  
  //To Do //See: DNSClient.FreeAddrInfo
 except
  on E: Exception do
   begin
    NetworkSetLastError(WSANO_RECOVERY);
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
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
 
  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  //To Do //See: DNSClient.GetNameInfo
 except
  on E: Exception do
   begin
    Result:=WSANO_RECOVERY;
    NetworkSetLastError(WSANO_RECOVERY);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: GetNameInfo ' + E.Message);
    {$ENDIF}
   end;
 end;
end;
  
{==============================================================================}
{==============================================================================}
{RTL Sockets Functions}
function fpsocket(domain:cint; xtype:cint; protocol: cint):clong;
begin
 {}
 Result:=clong(INVALID_SOCKET);
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;

  {Check Manager}
  NetworkSetLastError(WSASYSNOTREADY);
  if ProtocolManager = nil then Exit;
  
  {Create Socket}
  Result:=ProtocolManager.Socket(domain,xtype,protocol);
 except
  on E: Exception do
   begin
    Result:=clong(INVALID_SOCKET);
    NetworkSetLastError(WSAEPROTONOSUPPORT);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpsocket ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpsend(s:clong; msg:pointer; len:size_t; flags:cint):ssize_t;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpsend ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpsendto(s:clong; msg:pointer; len:size_t; flags:cint; tox :psockaddr; tolen: tsocklen):ssize_t;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpsendto ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fprecv(s:clong; buf: pointer; len: size_t; flags: cint):ssize_t;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fprecv ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fprecvfrom(s:clong; buf: pointer; len: size_t; flags: cint; from : psockaddr; fromlen : psocklen):ssize_t;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fprecvfrom ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpconnect(s:clong; name  : psockaddr; namelen : tsocklen):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpconnect ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpshutdown(s:clong; how:cint):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpshutdown ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpbind(s:clong; addrx : psockaddr; addrlen : tsocklen):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpbind ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fplisten(s:clong; backlog : cint):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fplisten ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpaccept(s:clong; addrx : psockaddr; addrlen : psocklen):clong;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=clong(INVALID_SOCKET);
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    Result:=clong(INVALID_SOCKET);
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpaccept ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpgetsockname(s:clong; name  : psockaddr; namelen : psocklen):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpgetsockname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpgetpeername(s:clong; name  : psockaddr; namelen : psocklen):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpgetpeername ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpgetsockopt(s:clong; level:cint; optname:cint; optval:pointer; optlen : psocklen):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpgetsockopt ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpsetsockopt(s:clong; level:cint; optname:cint; optval:pointer; optlen :tsocklen):cint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
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
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpsetsockopt ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpsocketpair(d:cint; xtype:cint; protocol:cint; sv:pclong):cint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}
{==============================================================================}
{RTL Select Function (Sockets only)}
function fpselect(n:cint; readfds, writefds, exceptfds: PFDSet; TimeOut: PTimeVal):cint;
{Note: All sockets contained by the FDSet must be of the same type}
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Manager}
  NetworkSetLastError(WSASYSNOTREADY);
  if ProtocolManager = nil then Exit;
  
  {Select Socket}
  Result:=ProtocolManager.Select(n,readfds,writefds,exceptfds,TimeOut);
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpselect ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpFD_SET(fdno:clong; var nset: TFDSet):cint;
begin
 {}
 Result:=-1;

 if nset.fd_count < FD_MAXFDSET then
  begin
   nset.fd_array[nset.fd_count]:=fdno;
   Inc(nset.fd_count);

   Result:=0;
  end;
end;

{==============================================================================}

function fpFD_CLR(fdno:clong; var nset: TFDSet):cint;
var
 I: Integer;
begin
 {}
 Result:=-1;
 
 I:=0;
 while I < nset.fd_count do
  begin
   if nset.fd_array[I] = fdno then
    begin
     while I < nset.fd_count - 1 do
      begin
       nset.fd_array[I]:=nset.fd_array[I + 1];
       Inc(I);
      end;
      
     Dec(nset.fd_count);
     
     Result:=0;
     Break;
    end;
    
   Inc(I);
  end;
end;

{==============================================================================}

function fpFD_ZERO(out nset: TFDSet):cint;
begin
 {}
 nset.fd_count:=0;

 Result:=0;
end;

{==============================================================================}

function fpFD_ISSET(fdno:clong; const nset: TFDSet): cint;
var
 I:Integer;
begin
 {}
 I:=nset.fd_count;
 while I > 0 do
  begin
   Dec(I);
   if nset.fd_array[I] = fdno then
    begin
     Result:=1;
     Exit;
    end;
  end;
  
 Result:=0;
end;

{==============================================================================}
{==============================================================================}
{RTL File/Text Sockets Functions}
function fpRead(Handle:clong;var BufPtr;Size:DWORD):DWORD;
var
 Available:DWORD;
 Socket:TProtocolSocket;
begin
 {}
 Result:=0;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
 
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(Handle);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(Handle,True,NETWORK_LOCK_READ) then Exit;
 
  {IOCTL Socket}
  if Socket.Protocol.IoctlSocket(Socket,FIONREAD,Available) = SOCKET_ERROR then Exit;
 
  if Available > 0 then
   begin
    if Size > Available then Size:=Available;
    
    {Receive Socket}
    Result:=DWORD(Socket.Protocol.Recv(Socket,BufPtr,Size,0));
    if Result = DWORD(SOCKET_ERROR) then Result:=0;
   end; 
 
  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpRead ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function fpWrite(Handle:clong;var BufPtr;Size:DWORD):DWORD;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=0;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if SocketsStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(Handle);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(Handle,True,NETWORK_LOCK_READ) then Exit;

  {Send Socket}
  Result:=DWORD(Socket.Protocol.Send(Socket,BufPtr,Size,0));
  if Result = DWORD(SOCKET_ERROR) then Result:=0;
  
  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF SOCKET_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Exception: fpWrite ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}
  
procedure OpenSock(var F:Text);
begin
 {}
 if Textrec(F).Handle=UnusedHandle then
  begin
   Textrec(F).Mode:=fmClosed;
  end 
 else
  begin
   case Textrec(F).userdata[1] of
    S_OUT:Textrec(F).Mode:=fmOutput;
    S_IN:Textrec(F).Mode:=fmInput;
   else
    Textrec(F).Mode:=fmClosed;
   end;
  end; 
end;

{==============================================================================}

procedure IOSock(var F:Text);
var
 Return:sizeint;
 DefaultError:Word;
begin
 {}
 with Textrec(F) do
  begin
   case Mode of
     fmOutput:begin
       repeat
        {$ifdef use_readwrite}
        Return:=fpWrite(Handle,BufPtr^,BufPos);
        {$else}
        Return:=fpSend(Handle,BufPtr,BufPos,0);
        {$endif}
       until (Return <> -1) or (SocketError <> EsockEINTR);
       
       BufEnd:=Return;
       DefaultError:=101; {File write error}
      end;
     fmInput:begin
       repeat
        {$ifdef use_readwrite}
        Return:=fpRead(Handle,BufPtr^,BufSize);
        {$else}
        Return:=fpRecv(Handle,BufPtr,BufSize,0);
        {$endif}
       until (Return <> -1) or (SocketError <> EsockEINTR);
       
       BufEnd:=Return;
       DefaultError:=100; {File read error}
      end;
   end;
   
   if Return = -1 then
    begin
     case SocketError of
      EsockEBADF,EsockENOTSOCK:InOutRes:=6; {Invalid file handle}
      EsockEFAULT:InOutRes:=217;
      EsockEINVAL:InOutRes:=218;
     else
      InOutRes:=DefaultError;
     end;
    end; 
     
   BufPos:=0;
  end;
end;

{==============================================================================}

procedure FlushSock(var F:Text);
begin
 {}
 if (Textrec(F).Mode = fmOutput) and (Textrec(F).BufPos <> 0) then
  begin
   IOSock(F);
   Textrec(F).BufPos:=0;
  end;
end;

{==============================================================================}

procedure CloseSock(var F:text);
begin
 {Nothing special has to be done here}
end;

{==============================================================================}

procedure Sock2Text(Sock:clong;Var SockIn,SockOut:Text); 
{Set up two Pascal Text file descriptors for reading and writing}
begin
 {First the reading part.}
 Assign(SockIn,'.');
 Textrec(SockIn).Handle:=Sock;
 Textrec(Sockin).userdata[1]:=S_IN;
 TextRec(SockIn).OpenFunc:=@OpenSock;
 TextRec(SockIn).InOutFunc:=@IOSock;
 TextRec(SockIn).FlushFunc:=@FlushSock;
 TextRec(SockIn).CloseFunc:=@CloseSock;
 TextRec(SockIn).Mode:=fmInput;
 
 case DefaultTextLineBreakStyle of
  tlbsLF:TextRec(SockIn).LineEnd:=#10;
  tlbsCRLF:TextRec(SockIn).LineEnd:=#13#10;
  tlbsCR:TextRec(SockIn).LineEnd:=#13;
 end;
 
 {Now the writing part}
 Assign(SockOut,'.');
 Textrec(SockOut).Handle:=Sock;
 Textrec(SockOut).userdata[1]:=S_OUT;
 TextRec(SockOut).OpenFunc:=@OpenSock;
 TextRec(SockOut).InOutFunc:=@IOSock;
 TextRec(SockOut).FlushFunc:=@FlushSock;
 TextRec(SockOut).CloseFunc:=@CloseSock;
 TextRec(SockOut).Mode:=fmOutput;

 case DefaultTextLineBreakStyle of
  tlbsLF:TextRec(SockOut).LineEnd:=#10;
  tlbsCRLF:TextRec(SockOut).LineEnd:=#13#10;
  tlbsCR:TextRec(SockOut).LineEnd:=#13;
 end;
end;

{==============================================================================}

function DoAccept(Sock:clong;var addr:TInetSockAddr):longint;
var
 AddrLen:Longint;
begin
 {}
 AddrLen:=SizeOf(Addr);
 repeat
  Result:=fpAccept(Sock,@Addr,@AddrLen);
 until (Result <> -1) or (SocketError <> EsockEINTR);
end;

{==============================================================================}

function Accept(Sock:clong;var addr:TInetSockAddr;var SockIn,SockOut:File):Boolean; 
var
 S:longint;
begin
 {}
 S:=DoAccept(Sock,addr);
 if S > 0 then
  begin
   Sock2File(S,SockIn,SockOut);
   Result:=True;
  end
 else
  begin
   Result:=False;
  end; 
end;

{==============================================================================}

function Accept(Sock:clong;var addr:TInetSockAddr;var SockIn,SockOut:text):Boolean; 
var
 S:longint;
begin
 {}
 S:=DoAccept(Sock,addr);
 if S > 0 then
  begin
   Sock2Text(S,SockIn,SockOut);
   Result:=True;
  end
 else
  begin
   Result:=False;
  end; 
end;

{==============================================================================}

function DoConnect(Sock:clong;const addr:TInetSockAddr):Boolean;
var
 Res:longint;
begin
 {}
 repeat
  Res:=fpConnect(Sock,@Addr,SizeOF(TInetSockAddr));
 until (Res <> -1) or (SocketError <> EsockEINTR);
 Result:=(Res = 0);
end;

{==============================================================================}

function Connect(Sock:clong;const addr:TInetSockAddr;var SockIn,SockOut:text):Boolean; 
begin
 {}
 Result:=DoConnect(Sock,addr);
 if Result then Sock2Text(Sock,SockIn,SockOut);
end;

{==============================================================================}

function Connect(Sock:clong;const addr:TInetSockAddr;var SockIn,SockOut:file):Boolean; 
begin
 {}
 Result:=DoConnect(Sock,addr);
 if Result then Sock2File(Sock,SockIn,SockOut);
end;

{==============================================================================}

procedure Sock2File(Sock:clong;Var SockIn,SockOut:File); 
begin
 {Input}
 Assign(SockIn,'.');
 FileRec(SockIn).Handle:=Sock;
 FileRec(SockIn).RecSize:=1;
 FileRec(Sockin).userdata[1]:=S_IN;
 FileRec(SockIn).Mode:=fmInput;

 {Output}
 Assign(SockOut,'.');
 FileRec(SockOut).Handle:=Sock;
 FileRec(SockOut).RecSize:=1;
 FileRec(SockOut).userdata[1]:=S_OUT;
 FileRec(SockOut).Mode:=fmOutput;
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

procedure SocketsProcessSocket(Data:Pointer);
begin
 {}
 try
  {$IFDEF SOCKET_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Process sockets');
  {$ENDIF}
 
  {Check Manager}
  if ProtocolManager = nil then Exit;
  
  {Process Sockets}
  ProtocolManager.ProcessSockets;
  
 finally
  {Enable Timer}
  TimerEnable(SocketsSocketTimer);
 end; 
end;

{==============================================================================}

procedure SocketsProcessAuth(Data:Pointer);
begin
 {}
 try
  {$IFDEF SOCKET_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Process authenticators');
  {$ENDIF}
 
  {Check Manager}
  if TransportManager = nil then Exit;
  
  {Process Authenticators}
  TransportManager.ProcessAuthenticators;
  
 finally
  {Enable Timer}
  TimerEnable(SocketsAuthTimer);
 end; 
end;

{==============================================================================}

procedure SocketsProcessMonitor(Data:Pointer);
begin
 {}
 try
  {$IFDEF SOCKET_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Process monitors');
  {$ENDIF}
 
  {Check Manager}
  if TransportManager = nil then Exit;
  
  {Process Monitors}
  TransportManager.ProcessMonitors;
  
 finally
  {Enable Timer}
  TimerEnable(SocketsMonitorTimer);
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
  
 {Check Settings}
 if NetworkSettings = nil then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(SocketsLock) = ERROR_SUCCESS then
  begin
   try
    {Check Started and Ready}
    if (SocketsStartupCount > 0) and (SocketsStartupError = ERROR_SUCCESS) then
     begin
      {Check Type}
      case Event.Device.Device.DeviceType of
       NETWORK_TYPE_ETHERNET,NETWORK_TYPE_TOKENRING:begin
         {Check Adapter}
         Adapter:=AdapterManager.GetAdapterByDevice(Event.Device,False,NETWORK_LOCK_NONE); {Do not lock}
         if Adapter = nil then
          begin
           {Create Adapter}
           if NetworkSettings.GetBooleanDefault('WIRED_NETWORK_ENABLED',WIRED_NETWORK_ENABLED) then 
            begin
             Adapter:=TWiredAdapter.Create(AdapterManager,Event.Device,DeviceGetName(@Event.Device.Device));
             
             {Check Adapter}
             if not NetworkSettings.GetBoolean(Adapter.Name + '_DISABLED') then
              begin
               {Start Adapter}
               Adapter.StartAdapter;
               
               {Bind Transports}
               TransportManager.BindTransports(Adapter);
               
               {Bind Monitors}
               TransportManager.BindMonitors(Adapter);
               
               {Bind Authenticators}
               TransportManager.BindAuthenticators(Adapter);
              end; 
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
    {Check Started and Ready}
    if (SocketsStartupCount > 0) and (SocketsStartupError = ERROR_SUCCESS) then
     begin
      {Check Type}
      case Network.Device.DeviceType of
       NETWORK_TYPE_ETHERNET,NETWORK_TYPE_TOKENRING:begin
         {Check Adapter}
         Adapter:=AdapterManager.GetAdapterByDevice(Network,True,NETWORK_LOCK_READ);
         if Adapter <> nil then
          begin
           {Unbind Authenticators}
           TransportManager.UnbindAuthenticators(Adapter);

           {Unbind Monitors}
           TransportManager.UnbindMonitors(Adapter);

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

function SocketsNetworkDeviceUp(Network:PNetworkDevice):LongWord;
var
 Adapter:TNetworkAdapter;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF SOCKET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Network device up');
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
    {Check Started and Ready}
    if (SocketsStartupCount > 0) and (SocketsStartupError = ERROR_SUCCESS) then
     begin
      {Check Type}
      case Network.Device.DeviceType of
       NETWORK_TYPE_ETHERNET,NETWORK_TYPE_TOKENRING:begin
         {Check Adapter}
         Adapter:=AdapterManager.GetAdapterByDevice(Network,True,NETWORK_LOCK_READ);
         if Adapter <> nil then
          begin
           {Update Status}
           Adapter.Status:=ADAPTER_STATUS_UP;
           
           {Unlock Adapter}
           Adapter.ReaderUnlock;
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

function SocketsNetworkDeviceDown(Network:PNetworkDevice):LongWord;
var
 Adapter:TNetworkAdapter;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF SOCKET_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Sockets: Network device down');
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
    {Check Started and Ready}
    if (SocketsStartupCount > 0) and (SocketsStartupError = ERROR_SUCCESS) then
     begin
      {Check Type}
      case Network.Device.DeviceType of
       NETWORK_TYPE_ETHERNET,NETWORK_TYPE_TOKENRING:begin
         {Check Adapter}
         Adapter:=AdapterManager.GetAdapterByDevice(Network,True,NETWORK_LOCK_READ);
         if Adapter <> nil then
          begin
           {Update Status}
           Adapter.Status:=ADAPTER_STATUS_DOWN;
 
           {Unlock Adapter}
           Adapter.ReaderUnlock;
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
 
 {Check Settings}
 if NetworkSettings = nil then Exit;
 
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
           if NetworkSettings.GetBooleanDefault('WIRED_NETWORK_ENABLED',WIRED_NETWORK_ENABLED) then 
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
 
 {Acquire the Lock}
 if CriticalSectionLock(SocketsLock) = ERROR_SUCCESS then
  begin
   try
    {Check Started}
    if SocketsStartupCount > 0 then
     begin
      {Check Notification}
      if (Notification and DEVICE_NOTIFICATION_REGISTER) <> 0 then
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
       end
      else if (Notification and DEVICE_NOTIFICATION_CLOSING) <> 0 then
       begin
        {Check Adapter}
        Adapter:=AdapterManager.GetAdapterByDevice(PNetworkDevice(Device),False,NETWORK_LOCK_NONE); {Do not lock}
        if Adapter <> nil then
         begin
          {Remove Adapter}
          SocketsNetworkDeviceRemove(PNetworkDevice(Device));
         end;
       end
      else if (Notification and DEVICE_NOTIFICATION_DEREGISTER) <> 0 then
       begin
        {Check Adapter}
        Adapter:=AdapterManager.GetAdapterByDevice(PNetworkDevice(Device),False,NETWORK_LOCK_NONE); {Do not lock}
        if Adapter <> nil then
         begin
          {Remove Adapter}
          SocketsNetworkDeviceRemove(PNetworkDevice(Device));
         end;
       end
      else if (Notification and DEVICE_NOTIFICATION_UP) <> 0 then
       begin
        {Check Adapter}
        Adapter:=AdapterManager.GetAdapterByDevice(PNetworkDevice(Device),False,NETWORK_LOCK_NONE); {Do not lock}
        if Adapter <> nil then
         begin
          {Update Status}
          SocketsNetworkDeviceUp(PNetworkDevice(Device));
         end;
       end
      else if (Notification and DEVICE_NOTIFICATION_DOWN) <> 0 then
       begin
        {Check Adapter}
        Adapter:=AdapterManager.GetAdapterByDevice(PNetworkDevice(Device),False,NETWORK_LOCK_NONE); {Do not lock}
        if Adapter <> nil then
         begin
          {Update Status}
          SocketsNetworkDeviceDown(PNetworkDevice(Device));
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
{==============================================================================}
{RTL Sockets Helper Functions}
function htonl(host:cardinal):cardinal; inline; 
begin
 {}
 {$IFDEF FPC_BIG_ENDIAN}
 Result:=host;
 {$ELSE}
 Result:=SwapEndian(host);
 {$ENDIF}
end;

{==============================================================================}

function ntohl(net:cardinal):cardinal; inline; 
begin
 {}
 {$IFDEF FPC_BIG_ENDIAN}
 Result:=net; 
 {$ELSE}
 Result:=SwapEndian(net);
 {$ENDIF}
end;

{==============================================================================}

function htons(host:word):word; inline;
begin
 {}
 {$IFDEF FPC_BIG_ENDIAN}
 Result:=host;
 {$ELSE}
 Result:=SwapEndian(host);
 {$ENDIF}
end;

{==============================================================================}

function ntohs(net:word):word; inline;
begin
 {}
 {$IFDEF FPC_BIG_ENDIAN}
 Result:=net;
 {$ELSE}
 Result:=SwapEndian(net);
 {$ENDIF}
end;

{==============================================================================}

type
 Array4Int = array[1..4] of Byte;

function NetAddrToStr(Entry:in_addr):AnsiString;
var 
 Count:LongInt;
 Value:LongInt;
 Dummy:AnsiString;
begin
 {}
 Result:='';
 
 Value:=Entry.S_addr;
 for Count:=1 to 4 do
  begin
   Str(Array4Int(Value)[Count],Dummy);
   Result:=Result + Dummy;
   if Count < 4 then Result:=Result + '.';
  end;
end;

{==============================================================================}

function HostAddrToStr(Entry:in_addr):AnsiString;
var
 Value:in_addr;
begin
 {}
 Value.S_addr:=htonl(Entry.S_addr);
 Result:=NetAddrToStr(Value);
end;

{==============================================================================}

function StrToHostAddr(IP:AnsiString):in_addr;
var
 Temp:in_addr;
 Count,Index,Value:LongInt;
 Dummy:AnsiString;
begin
 {}
 Result.S_addr:=0;  { :=NoAddress; }
 
 for Count:=1 to 4 do
  begin
   if Count < 4 then
    begin
     Index:=Pos('.',IP);
     if Index = 0 then Exit;
     
     Dummy:=Copy(IP,1,Index - 1);
     Delete(IP,1,Index);
    end
   else
    begin
     Dummy:=IP;
    end; 
       
   Val(Dummy,Value,Index);
   Array4Int(Temp.S_addr)[Count]:=Value;
   if Index <> 0 then Exit;
  end;
  
 Result.S_addr:=ntohl(Temp.S_addr);
end;

{==============================================================================}

function StrToNetAddr(IP:AnsiString):in_addr;
begin
 {}
 Result.S_addr:=htonl(StrToHostAddr(IP).S_addr);
end;

{==============================================================================}

function HostToNet(Host:in_addr):in_addr;
begin
 {}
 Result.S_addr:=htonl(Host.S_addr);
end;

{==============================================================================}

function NetToHost(Net:in_addr):in_addr; 
begin
 {}
  Result.S_addr:=ntohl(Net.S_addr);
end;

{==============================================================================}

function HostToNet(Host:Longint):Longint; 
begin
 {}
 Result:=htonl(Host);
end;

{==============================================================================}

function NetToHost(Net:Longint):Longint; 
begin
 {}
 Result:=ntohl(Net);
end;

{==============================================================================}

function ShortHostToNet(Host:Word):Word; 
begin
 {}
 Result:=htons(Host);
end;

{==============================================================================}

function ShortNetToHost(Net:Word):Word; 
begin
 {}
 Result:=ntohs(Net);
end;

{==============================================================================}

const 
 DigitTab:ShortString = ('0123456789ABCDEF');
 
function LocalIntToHex(Value:Integer;Digits:LongInt):AnsiString;
begin
 {}
 SetLength(Result,4);
 Result[4]:=DigitTab[1 +(Value and 15)];
 Result[3]:=DigitTab[1 +((Value shr 4) and 15)];
 Result[2]:=DigitTab[1 +((Value shr 8) and 15)];
 Result[1]:=DigitTab[1 +((Value shr 12) and 15)];
end;

{==============================================================================}

function HostAddrToStr6(Entry:Tin6_addr):AnsiString;
//To Do //This needs testing
var
 Count:Byte;
 ZC1,ZC2:Byte;
 ZR1,ZR2:set of Byte;
 Skipped:Boolean;
begin
 {}
 ZR1:=[];
 ZR2:=[];
 ZC1:=0;
 ZC2:=0;
 
 for Count:=0 to 7 do
  begin
   if Entry.u6_addr16[Count] = 0 then
    begin
     Include(ZR2,Count);
     Inc(ZC2);
    end
   else
    begin
     if ZC1 < ZC2 then
      begin
       ZC1:=ZC2;
       ZR1:=ZR2;
       ZC2:=0;
       ZR2:=[];
      end;
    end;
  end;
  
 if ZC1 < ZC2 then
  begin
   ZC1:=ZC2;
   ZR1:=ZR2;
  end;
  
 SetLength(Result,8 * 5 - 1);
 SetLength(Result,0);
 Skipped:=False;
 
 for Count:=0 to 7 do
  begin
   if not(Count in ZR1) then
    begin
     if Skipped then
      begin
       if Result = '' then Result:='::' else Result:=Result + ':';
         
       Skipped:=False;
      end;
     
     //FIXME: is that shortnettohost really proper there? I wouldn't be too sure...
     Result:=Result + LocalIntToHex(ShortNetToHost(Entry.u6_addr16[Count]),1) + ':';
    end
   else
    begin
     Skipped:=True;
    end;
  end;
  
 if Skipped then
  begin
   if Result = '' then Result:='::' else Result:=Result + ':';
  end;
  
 if Result = '' then Result:='::';
 
 if not(7 in ZR1) then SetLength(Result,Length(Result) - 1);
end;

{==============================================================================}

function StrToHostAddr6(IP:String):Tin6_addr; 
//To Do //This needs testing
var
 Part:String;
 Value:Word;
 Code:Integer;
 Index:Integer;
 ZeroAt:Integer;
 IPv6:TIn6_addr;
 Position:Integer;
Begin
 {}
 FillChar(IPv6,SizeOf(IPv6),0);
 
 { Every 16-bit block is converted at its own and stored into Result. When }
 { the '::' zero-spacer is found, its location is stored. Afterwards the   }
 { address is shifted and zero-filled.                                     }
 Index:=0;
 ZeroAt:=-1;
 Code:=0;
 Position:=Pos(':',IP);
 while (Position > 0) and (Length(IP) > 0) and (Index < 8) do
  begin
   Part:='$' + Copy(IP,1,Position - 1);
   Delete(IP,1,Position);
   
   if Length(Part) > 1 then Val(Part,Value,Code) else Value:=0; { is there a digit after the '$'? }
   
   IPv6.u6_addr16[Index]:=htons(Value);
   
   if Code <> 0 then
    begin
     FillChar(IPv6,SizeOf(IPv6),0);
     Exit;
    end;
    
   if IP[1] = ':' then
    begin
     ZeroAt:=Index;
     Delete(IP,1,1);
    end;
    
   Inc(Index);
   Position:=Pos(':',IP);
   if Position = 0 then Position:=Length(IP) + 1;
  end;
    
 { Address      a:b:c::f:g:h }
 { Result now   a : b : c : f : g : h : 0 : 0, ZeroAt = 2, Index = 6 }
 { Result after a : b : c : 0 : 0 : f : g : h }
 if ZeroAt >= 0 then
  begin
   System.Move(IPv6.u6_addr16[ZeroAt + 1],IPv6.u6_addr16[(8 - Index) + ZeroAt + 1],2 * (Index - ZeroAt - 1));
   FillChar(IPv6.u6_addr16[ZeroAt + 1],2 * (8 - Index),0);
  end;

 Result:=IPv6;
End;

{==============================================================================}

function NetAddrToStr6(Entry:Tin6_addr):AnsiString;
begin
 {}
 Result:=HostAddrToStr6(Entry);
end;

{==============================================================================}

function StrToNetAddr6(IP:AnsiString):TIn6_Addr;
begin
 {}
 Result:=StrToHostAddr6(IP);
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
     WorkerSchedule(SOCKETS_STARTDELAY,TWorkerTask(SocketsAsyncStart),nil,nil); {Delay start to allow device initialization}
    end;
  end; 
 
{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end. 