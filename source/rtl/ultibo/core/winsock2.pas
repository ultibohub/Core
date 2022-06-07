{
Ultibo Winsock2 interface unit.

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


Winsock 2
=========

 Notes: All BSD/Winsock functions that accept an Address or Port expect
        them to be in Network order. All other functions that take an
        Address or Port expect them to be in Host order

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Winsock2;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,Platform,Threads,SysUtils,Classes,UltiboClasses,UltiboUtils,Network,Transport,Protocol,Sockets,
     Loopback,ARP,IP,IPv6,UDP,TCP,ICMP,ICMPv6,IGMP,RAW,DHCP,DNS;
      
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
      
{==============================================================================}
const
 {Winsock2 specific constants}
 WINSOCK_VERSION = $0202;
 
 WINSOCK_TCP_SERVER_THREAD_NAME = 'TCP Server';                 {Thread name for TCP server threads}
 WINSOCK_TCP_SERVER_THREAD_PRIORITY = THREAD_PRIORITY_NORMAL;   {Thread priority for TCP server threads}
 
 WINSOCK_TCP_LISTENER_THREAD_NAME = 'TCP Listener';             {Thread name for TCP listener threads}
 WINSOCK_TCP_LISTENER_THREAD_PRIORITY = THREAD_PRIORITY_NORMAL; {Thread priority for TCP listener threads}
 
 WINSOCK_UDP_SERVER_THREAD_NAME = 'UDP Server';                 {Thread name for UDP server threads}
 WINSOCK_UDP_SERVER_THREAD_PRIORITY = THREAD_PRIORITY_NORMAL;   {Thread priority for UDP server threads}
 
 WINSOCK_UDP_LISTENER_THREAD_NAME = 'UDP Listener';             {Thread name for UDP listener threads}
 WINSOCK_UDP_LISTENER_THREAD_PRIORITY = THREAD_PRIORITY_NORMAL; {Thread priority for UDP listener threads}
 
 FD_SETSIZE = GlobalSock.FD_SETSIZE;
 
const
 IOCPARM_MASK = GlobalSock.IOCPARM_MASK;
 IOC_VOID     = GlobalSock.IOC_VOID;
 IOC_OUT      = GlobalSock.IOC_OUT;
 IOC_IN       = GlobalSock.IOC_IN;
 IOC_INOUT    = GlobalSock.IOC_INOUT;

 FIONREAD = GlobalSock.FIONREAD; 
 FIONBIO = GlobalSock.FIONBIO;
 FIOASYNC = GlobalSock.FIOASYNC;

 SIOCSHIWAT = GlobalSock.SIOCSHIWAT;
 SIOCGHIWAT = GlobalSock.SIOCGHIWAT;
 SIOCSLOWAT = GlobalSock.SIOCSLOWAT;
 SIOCGLOWAT = GlobalSock.SIOCGLOWAT;
 SIOCATMARK = GlobalSock.SIOCATMARK;
 
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

{ Link numbers }
 IMPLINK_IP         =  GlobalSock.IMPLINK_IP;
 IMPLINK_LOWEXPER   =  GlobalSock.IMPLINK_LOWEXPER;
 IMPLINK_HIGHEXPER  =  GlobalSock.IMPLINK_HIGHEXPER;
 
const
 TF_DISCONNECT           = GlobalSock.TF_DISCONNECT;
 TF_REUSE_SOCKET         = GlobalSock.TF_REUSE_SOCKET;
 TF_WRITE_BEHIND         = GlobalSock.TF_WRITE_BEHIND;

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

 IP_DEFAULT_MULTICAST_TTL   = GlobalSock.IP_DEFAULT_MULTICAST_TTL;
 IP_DEFAULT_MULTICAST_LOOP  = GlobalSock.IP_DEFAULT_MULTICAST_LOOP;
 IP_MAX_MEMBERSHIPS         = GlobalSock.IP_MAX_MEMBERSHIPS;

{ This is used instead of -1, since the
  TSocket type is unsigned.}
 INVALID_SOCKET         = GlobalSock.INVALID_SOCKET;
 SOCKET_ERROR           = GlobalSock.SOCKET_ERROR;

{ Types }
 SOCK_UNSPEC     = GlobalSock.SOCK_UNSPEC;
 SOCK_STREAM     = GlobalSock.SOCK_STREAM;
 SOCK_DGRAM      = GlobalSock.SOCK_DGRAM;
 SOCK_RAW        = GlobalSock.SOCK_RAW;
 SOCK_RDM        = GlobalSock.SOCK_RDM;
 SOCK_SEQPACKET  = GlobalSock.SOCK_SEQPACKET;
 SOCK_PACKET     = GlobalSock.SOCK_PACKET;

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
 INADDR_ANY       = GlobalSock.INADDR_ANY;
 INADDR_LOOPBACK  = GlobalSock.INADDR_LOOPBACK;
 INADDR_BROADCAST = GlobalSock.INADDR_BROADCAST;
 INADDR_NONE      = GlobalSock.INADDR_NONE;
 
 IN6ADDR_ANY:TIn6Addr = (u6_addr16: (0, 0, 0, 0, 0, 0, 0, 0));
 IN6ADDR_LOOPBACK:TIn6Addr = (u6_addr8: (0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1));
 
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

{ Define constant based on rfc883, used by gethostbyxxxx() calls. }
 MAXGETHOSTSTRUCT        = GlobalSock.MAXGETHOSTSTRUCT;
 
const
{ WinSock 2 extension -- bit values and indices for FD_XXX network events }
 FD_READ_BIT = 0;
 FD_READ = (1  shl  FD_READ_BIT);
 FD_WRITE_BIT = 1;
 FD_WRITE = (1  shl  FD_WRITE_BIT);
 FD_OOB_BIT = 2;
 FD_OOB = (1  shl  FD_OOB_BIT);
 FD_ACCEPT_BIT = 3;
 FD_ACCEPT = (1  shl  FD_ACCEPT_BIT);
 FD_CONNECT_BIT = 4;
 FD_CONNECT = (1  shl  FD_CONNECT_BIT);
 FD_CLOSE_BIT = 5;
 FD_CLOSE = (1  shl  FD_CLOSE_BIT);
 FD_QOS_BIT = 6;
 FD_QOS = (1  shl  FD_QOS_BIT);
 FD_GROUP_QOS_BIT = 7;
 FD_GROUP_QOS = (1  shl  FD_GROUP_QOS_BIT);
 FD_MAX_EVENTS = 8;
 FD_ALL_EVENTS = ((1  shl  FD_MAX_EVENTS) - 1);
 
const
{ All Windows Sockets error constants are biased by WSABASEERR from the "normal"}
 WSABASEERR              = GlobalSock.WSABASEERR;

{ Windows Sockets definitions of regular Microsoft C error constants }

 WSAEINTR                = GlobalSock.WSAEINTR;
 WSAEBADF                = GlobalSock.WSAEBADF;
 WSAEACCES               = GlobalSock.WSAEACCES;
 WSAEFAULT               = GlobalSock.WSAEFAULT;
 WSAEINVAL               = GlobalSock.WSAEINVAL;
 WSAEMFILE               = GlobalSock.WSAEMFILE;

{ Windows Sockets definitions of regular Berkeley error constants }

 WSAEWOULDBLOCK          = GlobalSock.WSAEWOULDBLOCK;
 WSAEINPROGRESS          = GlobalSock.WSAEINPROGRESS;
 WSAEALREADY             = GlobalSock.WSAEALREADY;
 WSAENOTSOCK             = GlobalSock.WSAENOTSOCK;
 WSAEDESTADDRREQ         = GlobalSock.WSAEDESTADDRREQ;
 WSAEMSGSIZE             = GlobalSock.WSAEMSGSIZE;
 WSAEPROTOTYPE           = GlobalSock.WSAEPROTOTYPE;
 WSAENOPROTOOPT          = GlobalSock.WSAENOPROTOOPT;
 WSAEPROTONOSUPPORT      = GlobalSock.WSAEPROTONOSUPPORT;
 WSAESOCKTNOSUPPORT      = GlobalSock.WSAESOCKTNOSUPPORT;
 WSAEOPNOTSUPP           = GlobalSock.WSAEOPNOTSUPP;
 WSAEPFNOSUPPORT         = GlobalSock.WSAEPFNOSUPPORT;
 WSAEAFNOSUPPORT         = GlobalSock.WSAEAFNOSUPPORT;
 WSAEADDRINUSE           = GlobalSock.WSAEADDRINUSE;
 WSAEADDRNOTAVAIL        = GlobalSock.WSAEADDRNOTAVAIL;
 WSAENETDOWN             = GlobalSock.WSAENETDOWN;
 WSAENETUNREACH          = GlobalSock.WSAENETUNREACH;
 WSAENETRESET            = GlobalSock.WSAENETRESET;
 WSAECONNABORTED         = GlobalSock.WSAECONNABORTED;
 WSAECONNRESET           = GlobalSock.WSAECONNRESET;
 WSAENOBUFS              = GlobalSock.WSAENOBUFS;
 WSAEISCONN              = GlobalSock.WSAEISCONN;
 WSAENOTCONN             = GlobalSock.WSAENOTCONN;
 WSAESHUTDOWN            = GlobalSock.WSAESHUTDOWN;
 WSAETOOMANYREFS         = GlobalSock.WSAETOOMANYREFS;
 WSAETIMEDOUT            = GlobalSock.WSAETIMEDOUT;
 WSAECONNREFUSED         = GlobalSock.WSAECONNREFUSED;
 WSAELOOP                = GlobalSock.WSAELOOP;
 WSAENAMETOOLONG         = GlobalSock.WSAENAMETOOLONG;
 WSAEHOSTDOWN            = GlobalSock.WSAEHOSTDOWN;
 WSAEHOSTUNREACH         = GlobalSock.WSAEHOSTUNREACH;
 WSAENOTEMPTY            = GlobalSock.WSAENOTEMPTY;
 WSAEPROCLIM             = GlobalSock.WSAEPROCLIM;
 WSAEUSERS               = GlobalSock.WSAEUSERS;
 WSAEDQUOT               = GlobalSock.WSAEDQUOT;
 WSAESTALE               = GlobalSock.WSAESTALE;
 WSAEREMOTE              = GlobalSock.WSAEREMOTE;

 WSAEDISCON              = GlobalSock.WSAEDISCON;

{ Extended Windows Sockets error constant definitions }

 WSASYSNOTREADY          = GlobalSock.WSASYSNOTREADY;
 WSAVERNOTSUPPORTED      = GlobalSock.WSAVERNOTSUPPORTED;
 WSANOTINITIALISED       = GlobalSock.WSANOTINITIALISED;
 
 WSAENOMORE              = GlobalSock.WSAENOMORE;
 WSAECANCELLED           = GlobalSock.WSAECANCELLED;
 WSAEINVALIDPROCTABLE    = GlobalSock.WSAEINVALIDPROCTABLE;
 WSAEINVALIDPROVIDER     = GlobalSock.WSAEINVALIDPROVIDER;
 WSAEPROVIDERFAILEDINIT  = GlobalSock.WSAEPROVIDERFAILEDINIT;
 WSASYSCALLFAILURE       = GlobalSock.WSASYSCALLFAILURE;
 WSASERVICE_NOT_FOUND    = GlobalSock.WSASERVICE_NOT_FOUND;
 WSATYPE_NOT_FOUND       = GlobalSock.WSATYPE_NOT_FOUND;
 WSA_E_NO_MORE           = GlobalSock.WSA_E_NO_MORE;
 WSA_E_CANCELLED         = GlobalSock.WSA_E_CANCELLED;
 WSAEREFUSED             = GlobalSock.WSAEREFUSED;
 
{ Error return codes from gethostbyname() and gethostbyaddr()
  (when using the resolver). Note that these errors are
  retrieved via WSAGetLastError() and must therefore follow
  the rules for avoiding clashes with error numbers from
  specific implementations or language run-time systems.
  For this reason the codes are based at WSABASEERR+1001.
  Note also that [WSA]NO_ADDRESS is defined only for
  compatibility purposes. }

{ Authoritative Answer: Host not found }

 WSAHOST_NOT_FOUND       = GlobalSock.WSAHOST_NOT_FOUND;
 HOST_NOT_FOUND          = GlobalSock.HOST_NOT_FOUND;

{ Non-Authoritative: Host not found, or SERVERFAIL }

 WSATRY_AGAIN            = GlobalSock.WSATRY_AGAIN;
 TRY_AGAIN               = GlobalSock.TRY_AGAIN;

{ Non recoverable errors, FORMERR, REFUSED, NOTIMP }

 WSANO_RECOVERY          = GlobalSock.WSANO_RECOVERY;
 NO_RECOVERY             = GlobalSock.NO_RECOVERY;

{ Valid name, no data record of requested type }

 WSANO_DATA              = GlobalSock.WSANO_DATA;
 NO_DATA                 = GlobalSock.NO_DATA;

{ no address, look for MX record }

 WSANO_ADDRESS           = GlobalSock.WSANO_ADDRESS;
 NO_ADDRESS              = GlobalSock.NO_ADDRESS;
 
const
{ WinSock 2 extension -- new error codes and type definition }
 WSA_IO_PENDING = GlobalSock.WSA_IO_PENDING;
 WSA_IO_INCOMPLETE = GlobalSock.WSA_IO_INCOMPLETE;
 WSA_INVALID_HANDLE = GlobalSock.WSA_INVALID_HANDLE;
 WSA_INVALID_PARAMETER = GlobalSock.WSA_INVALID_PARAMETER;
 WSA_NOT_ENOUGH_MEMORY = GlobalSock.WSA_NOT_ENOUGH_MEMORY;
 WSA_OPERATION_ABORTED = GlobalSock.WSA_OPERATION_ABORTED;
 WSA_INVALID_EVENT = GlobalSock.WSA_INVALID_EVENT;
 WSA_MAXIMUM_WAIT_EVENTS = GlobalSock.WSA_MAXIMUM_WAIT_EVENTS;
 WSA_WAIT_FAILED = GlobalSock.WSA_WAIT_FAILED;
 WSA_WAIT_EVENT_0 = GlobalSock.WSA_WAIT_EVENT_0;
 WSA_WAIT_IO_COMPLETION = GlobalSock.WSA_WAIT_IO_COMPLETION;
 WSA_WAIT_TIMEOUT = GlobalSock.WSA_WAIT_TIMEOUT;
 WSA_INFINITE = GlobalSock.WSA_INFINITE;
 
{ Windows Sockets errors redefined as regular Berkeley error constants.
  These are commented out in Windows NT to avoid conflicts with errno.h.
  Use the WSA constants instead. }

 EWOULDBLOCK        =  GlobalSock.EWOULDBLOCK;
 EINPROGRESS        =  GlobalSock.EINPROGRESS;
 EALREADY           =  GlobalSock.EALREADY;
 ENOTSOCK           =  GlobalSock.ENOTSOCK;
 EDESTADDRREQ       =  GlobalSock.EDESTADDRREQ;
 EMSGSIZE           =  GlobalSock.EMSGSIZE;
 EPROTOTYPE         =  GlobalSock.EPROTOTYPE;
 ENOPROTOOPT        =  GlobalSock.ENOPROTOOPT;
 EPROTONOSUPPORT    =  GlobalSock.EPROTONOSUPPORT;
 ESOCKTNOSUPPORT    =  GlobalSock.ESOCKTNOSUPPORT;
 EOPNOTSUPP         =  GlobalSock.EOPNOTSUPP;
 EPFNOSUPPORT       =  GlobalSock.EPFNOSUPPORT;
 EAFNOSUPPORT       =  GlobalSock.EAFNOSUPPORT;
 EADDRINUSE         =  GlobalSock.EADDRINUSE;
 EADDRNOTAVAIL      =  GlobalSock.EADDRNOTAVAIL;
 ENETDOWN           =  GlobalSock.ENETDOWN;
 ENETUNREACH        =  GlobalSock.ENETUNREACH;
 ENETRESET          =  GlobalSock.ENETRESET;
 ECONNABORTED       =  GlobalSock.ECONNABORTED;
 ECONNRESET         =  GlobalSock.ECONNRESET;
 ENOBUFS            =  GlobalSock.ENOBUFS;
 EISCONN            =  GlobalSock.EISCONN;
 ENOTCONN           =  GlobalSock.ENOTCONN;
 ESHUTDOWN          =  GlobalSock.ESHUTDOWN;
 ETOOMANYREFS       =  GlobalSock.ETOOMANYREFS;
 ETIMEDOUT          =  GlobalSock.ETIMEDOUT;
 ECONNREFUSED       =  GlobalSock.ECONNREFUSED;
 ELOOP              =  GlobalSock.ELOOP;
 ENAMETOOLONG       =  GlobalSock.ENAMETOOLONG;
 EHOSTDOWN          =  GlobalSock.EHOSTDOWN;
 EHOSTUNREACH       =  GlobalSock.EHOSTUNREACH;
 ENOTEMPTY          =  GlobalSock.ENOTEMPTY;
 EPROCLIM           =  GlobalSock.EPROCLIM;
 EUSERS             =  GlobalSock.EUSERS;
 EDQUOT             =  GlobalSock.EDQUOT;
 ESTALE             =  GlobalSock.ESTALE;
 EREMOTE            =  GlobalSock.EREMOTE;

 ENOTREADY          =  GlobalSock.ENOTREADY;
 EVERNOTSUPPORTED   =  GlobalSock.EVERNOTSUPPORTED;
 ENOTINITIALISED    =  GlobalSock.ENOTINITIALISED;
 
const
 WSADESCRIPTION_LEN     =   GlobalSock.WSADESCRIPTION_LEN;
 WSASYS_STATUS_LEN      =   GlobalSock.WSASYS_STATUS_LEN;
 MAX_PROTOCOL_CHAIN     = 7;
 BASE_PROTOCOL          = 1;
 LAYERED_PROTOCOL       = 0;
 WSAPROTOCOL_LEN        = 255;
 
const
{ WinSock 2 extension -- WSABUF and QOS struct, include qos.h to pull in FLOWSPEC and related definitions }
 SERVICETYPE_NOTRAFFIC             =  $00000000;  // No data in this direction
 SERVICETYPE_BESTEFFORT            =  $00000001;  // Best Effort
 SERVICETYPE_CONTROLLEDLOAD        =  $00000002;  // Controlled Load
 SERVICETYPE_GUARANTEED            =  $00000003;  // Guaranteed
 SERVICETYPE_NETWORK_UNAVAILABLE   =  $00000004;  // Used to notify change to user
 SERVICETYPE_GENERAL_INFORMATION   =  $00000005;  // corresponds to "General Parameters" defined by IntServ
 SERVICETYPE_NOCHANGE              =  $00000006;  // used to indicate that the flow spec contains no change from any previous one
 
 SERVICE_IMMEDIATE_TRAFFIC_CONTROL =  $80000000; // to turn on immediate traffic control, OR this flag with the ServiceType field in teh FLOWSPEC

{ WinSock 2 extension -- manifest constants for return values of the condition function }
 CF_ACCEPT = $0000;
 CF_REJECT = $0001;
 CF_DEFER = $0002;

{ WinSock 2 extension -- manifest constants for shutdown() }
 SD_RECEIVE = GlobalSock.SD_RECEIVE;
 SD_SEND = GlobalSock.SD_SEND;
 SD_BOTH = GlobalSock.SD_BOTH;

{ WinSock 2 extension -- data type and manifest constants for socket groups }
 SG_UNCONSTRAINED_GROUP = $01;
 SG_CONSTRAINED_GROUP = $02;
 
const
{ Flag bit definitions for dwProviderFlags }
 PFL_MULTIPLE_PROTO_ENTRIES = $00000001;
 PFL_RECOMMENDED_PROTO_ENTRY = $00000002;
 PFL_HIDDEN = $00000004;
 PFL_MATCHES_PROTOCOL_ZERO = $00000008;

{ Flag bit definitions for dwServiceFlags1 }
 XP1_CONNECTIONLESS = $00000001;
 XP1_GUARANTEED_DELIVERY = $00000002;
 XP1_GUARANTEED_ORDER = $00000004;
 XP1_MESSAGE_ORIENTED = $00000008;
 XP1_PSEUDO_STREAM = $00000010;
 XP1_GRACEFUL_CLOSE = $00000020;
 XP1_EXPEDITED_DATA = $00000040;
 XP1_CONNECT_DATA = $00000080;
 XP1_DISCONNECT_DATA = $00000100;
 XP1_SUPPORT_BROADCAST = $00000200;
 XP1_SUPPORT_MULTIPOINT = $00000400;
 XP1_MULTIPOINT_CONTROL_PLANE = $00000800;
 XP1_MULTIPOINT_DATA_PLANE = $00001000;
 XP1_QOS_SUPPORTED = $00002000;
 XP1_INTERRUPT = $00004000;
 XP1_UNI_SEND = $00008000;
 XP1_UNI_RECV = $00010000;
 XP1_IFS_HANDLES = $00020000;
 XP1_PARTIAL_MESSAGE = $00040000;

 BIGENDIAN = $0000;
 LITTLEENDIAN = $0001;

 SECURITY_PROTOCOL_NONE = $0000;

{ WinSock 2 extension -- manifest constants for WSAJoinLeaf() }
 JL_SENDER_ONLY = $01;
 JL_RECEIVER_ONLY = $02;
 JL_BOTH = $04;

{ WinSock 2 extension -- manifest constants for WSASocket() }
 WSA_FLAG_OVERLAPPED = $01;
 WSA_FLAG_MULTIPOINT_C_ROOT = $02;
 WSA_FLAG_MULTIPOINT_C_LEAF = $04;
 WSA_FLAG_MULTIPOINT_D_ROOT = $08;
 WSA_FLAG_MULTIPOINT_D_LEAF = $10;

{ WinSock 2 extension -- manifest constants for WSAIoctl() }
 IOC_UNIX = $00000000;
 IOC_WS2 = $08000000;
 IOC_PROTOCOL = $10000000;
 IOC_VENDOR = $18000000;

 SIO_ASSOCIATE_HANDLE = IOC_IN or IOC_WS2 or 1;
 SIO_ENABLE_CIRCULAR_QUEUEING = IOC_WS2 or 2;
 SIO_FIND_ROUTE = IOC_OUT or IOC_WS2 or 3;
 SIO_FLUSH = IOC_WS2 or 4;
 SIO_GET_BROADCAST_ADDRESS = IOC_OUT or IOC_WS2 or 5;
 SIO_GET_EXTENSION_FUNCTION_POINTER = IOC_INOUT or IOC_WS2 or 6;
 SIO_GET_QOS = IOC_INOUT or IOC_WS2 or 7;
 SIO_GET_GROUP_QOS = IOC_INOUT or IOC_WS2 or 8;
 SIO_MULTIPOINT_LOOPBACK = IOC_IN or IOC_WS2 or 9;
 SIO_MULTICAST_SCOPE = IOC_IN or IOC_WS2 or 10;
 SIO_SET_QOS = IOC_IN or IOC_WS2 or 11;
 SIO_SET_GROUP_QOS = IOC_IN or IOC_WS2 or 12;
 SIO_TRANSLATE_HANDLE = IOC_INOUT or IOC_WS2 or 13;

{ WinSock 2 extension -- manifest constants for SIO_TRANSLATE_HANDLE ioctl }
 TH_NETDEV = $00000001;
 TH_TAPI = $00000002;
 
const
 SERVICE_MULTIPLE = $00000001;

{ Name Spaces }
 NS_ALL = (0);

 NS_SAP = (1);
 NS_NDS = (2);
 NS_PEER_BROWSE = (3);

 NS_TCPIP_LOCAL = (10);
 NS_TCPIP_HOSTS = (11);
 NS_DNS = (12);
 NS_NETBT = (13);
 NS_WINS = (14);

 NS_NBP = (20);

 NS_MS = (30);
 NS_STDA = (31);
 NS_NTDS = (32);

 NS_X500 = (40);
 NS_NIS = (41);
 NS_NISPLUS = (42);

 NS_WRQ = (50);

{ Resolution flags for WSAGetAddressByName() Note these are also used by the 1.1 API GetAddressByName, so leave them around. }
 RES_UNUSED_1 = $00000001;
 RES_FLUSH_CACHE = $00000002;
 RES_SERVICE = $00000004;

{ Well known value names for Service Types }
 SERVICE_TYPE_VALUE_IPXPORTA = 'IpxSocket';
 //SERVICE_TYPE_VALUE_IPXPORTW : PWideChar = 'IpxSocket';

 SERVICE_TYPE_VALUE_SAPIDA = 'SapId';
 //SERVICE_TYPE_VALUE_SAPIDW : PWideChar = 'SapId';

 SERVICE_TYPE_VALUE_TCPPORTA = 'TcpPort';
 //SERVICE_TYPE_VALUE_TCPPORTW : PWideChar = 'TcpPort';

 SERVICE_TYPE_VALUE_UDPPORTA = 'UdpPort';
 //SERVICE_TYPE_VALUE_UDPPORTW : PWideChar = 'UdpPort';

 SERVICE_TYPE_VALUE_OBJECTIDA = 'ObjectId';
 //SERVICE_TYPE_VALUE_OBJECTIDW : PWideChar = 'ObjectId';

 SERVICE_TYPE_VALUE_SAPID = SERVICE_TYPE_VALUE_SAPIDA;
 SERVICE_TYPE_VALUE_TCPPORT = SERVICE_TYPE_VALUE_TCPPORTA;
 SERVICE_TYPE_VALUE_UDPPORT = SERVICE_TYPE_VALUE_UDPPORTA;
 SERVICE_TYPE_VALUE_OBJECTID = SERVICE_TYPE_VALUE_OBJECTIDA;
 
const
 LUP_DEEP = $0001;
 LUP_CONTAINERS = $0002;
 LUP_NOCONTAINERS = $0004;
 LUP_NEAREST = $0008;
 LUP_RETURN_NAME = $0010;
 LUP_RETURN_TYPE = $0020;
 LUP_RETURN_VERSION = $0040;
 LUP_RETURN_COMMENT = $0080;
 LUP_RETURN_ADDR = $0100;
 LUP_RETURN_BLOB = $0200;
 LUP_RETURN_ALIASES = $0400;
 LUP_RETURN_QUERY_STRING = $0800;
 LUP_RETURN_ALL = $0FF0;
 LUP_RES_SERVICE = $8000;

 LUP_FLUSHCACHE = $1000;
 LUP_FLUSHPREVIOUS = $2000;


{ Return flags }
 RESULT_IS_ALIAS = $0001;
 
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
 {Winsock2 specific types}
 u_char = GlobalSock.u_char;
 u_short = GlobalSock.u_short;
 u_int = GlobalSock.u_int;
 u_long = GlobalSock.u_long;
 pu_char = GlobalSock.pu_char;
 pu_short = GlobalSock.pu_short;
 pu_int = GlobalSock.pu_int;
 pu_long = GlobalSock.pu_long;

 TSocket = GlobalSock.TSocket;

type
 WSAEVENT = GlobalSock.WSAEVENT;
 PWSAEVENT = GlobalSock.PWSAEVENT;
 LPWSAEVENT = GlobalSock.LPWSAEVENT;
 PMBChar = GlobalSock.PMBChar;
 
type
 PFDSet = GlobalSock.PFDSet;
 fdset = GlobalSock.fdset;
 TFDSet = GlobalSock.TFDSet;
 
 PTimeVal = GlobalSock.PTimeVal;
 timeval = GlobalSock.timeval;
 TTimeVal = GlobalSock.TTimeVal;
 
type
 PHostEnt = GlobalSock.PHostEnt;
 hostent = GlobalSock.hostent;
 THostEnt = GlobalSock.THostEnt;

 PNetEnt = GlobalSock.PNetEnt;
 netent = GlobalSock.netent;
 TNetEnt = GlobalSock.TNetEnt;

 PServEnt = GlobalSock.PServEnt;
 servent = GlobalSock.servent;
 TServEnt = GlobalSock.TServEnt;

 PProtoEnt = GlobalSock.PProtoEnt;
 protoent = GlobalSock.protoent;
 TProtoEnt = GlobalSock.TProtoEnt;
 
type
 SunB = GlobalSock.SunB;

 SunW = GlobalSock.SunW;

 PInAddr = GlobalSock.PInAddr;
 in_addr = GlobalSock.in_addr;
 TInAddr = GlobalSock.TInAddr;

 {IPv6 version of above}
 PIn6Addr = GlobalSock.PIn6Addr;
 in6_addr = GlobalSock.in6_addr;
 TIn6Addr = GlobalSock.TIn6Addr;

 {IPX version of above}
 PIpxAddr = GlobalSock.PIpxAddr;
 ipx_addr = GlobalSock.ipx_addr;
 TIpxAddr = GlobalSock.TIpxAddr;

 PSockAddrIn = GlobalSock.PSockAddrIn;
 sockaddr_in = GlobalSock.sockaddr_in;
 TSockAddrIn = GlobalSock.TSockAddrIn;
 
 {IPv6 version of above}
 PSockAddrIn6 = GlobalSock.PSockAddrIn6;
 sockaddr_in6 = GlobalSock.sockaddr_in6;
 TSockAddrIn6 = GlobalSock.TSockAddrIn6;

 {IPX version of above}
 PSockAddrIpx = GlobalSock.PSockAddrIpx;
 TSockAddrIpx = GlobalSock.TSockAddrIpx;
 
type
 { Structure used by kernel to store most addresses. }
 PSOCKADDR = GlobalSock.PSOCKADDR;
 TSockAddr = GlobalSock.TSockAddr;

 {IPv6 version of above}
 PSOCKADDR6 = GlobalSock.PSOCKADDR6;
 TSockAddr6 = GlobalSock.TSockAddr6;
 
 {IPX version of above}
 {PSOCKADDRIPX = GlobalSock.PSOCKADDRIPX;}
 {TSockAddrIpx = GlobalSock.TSockAddrIpx;}
 
 { Structure used by kernel to pass protocol information in raw sockets. }
 PSockProto = GlobalSock.PSockProto;
 sockproto = GlobalSock.sockproto;
 TSockProto = GlobalSock.TSockProto;
 
type
{ Structure used for manipulating linger option. }
 PLinger = GlobalSock.PLinger;
 linger = GlobalSock.linger;
 TLinger = GlobalSock.TLinger;
 
type
 PWSAData = GlobalSock.PWSAData;
 WSAData = GlobalSock.WSAData;
 TWSAData = GlobalSock.TWSAData;
 
 WSAOVERLAPPED = TOverlapped;
 TWSAOverlapped = WSAOverlapped;
 PWSAOverlapped = ^WSAOverlapped;
 LPWSAOVERLAPPED = PWSAOverlapped;
 
type
 BLOB = record
  cbSize : ULONG;
  pBlobData : ^BYTE;
 end;
 _BLOB = BLOB;
 TBLOB = BLOB;
 PBLOB = ^BLOB;
 
type
{ WinSock 2 extension -- WSABUF and QOS struct, include qos.h to pull in FLOWSPEC and related definitions }
 WSABUF = record
  len: U_LONG;    { the length of the buffer }
  buf: PChar;     { the pointer to the buffer }
 end {WSABUF};
 PWSABUF = ^WSABUF;
 LPWSABUF = PWSABUF;

 TServiceType = LongInt;

 TFlowSpec = Record
  TokenRate,               // In Bytes/sec
  TokenBucketSize,         // In Bytes
  PeakBandwidth,           // In Bytes/sec
  Latency,                 // In microseconds
  DelayVariation : LongInt;// In microseconds
  ServiceType : TServiceType;
  MaxSduSize,     MinimumPolicedSize : LongInt;// In Bytes
 end;
 PFlowSpec = ^TFLOWSPEC;
 flowspec = TFlowSpec;

 TQualityOfService = record
  SendingFlowspec: TFlowSpec;     { the flow spec for data sending }
  ReceivingFlowspec: TFlowSpec;   { the flow spec for data receiving }
  ProviderSpecific: WSABUF; { additional provider specific stuff }
 end {TQualityOfService};
 PQOS = ^TQualityOfService;
 LPQOS = PQOS;
 
type
 GROUP = u_long;

{ WinSock 2 extension -- data type for WSAEnumNetworkEvents() }
 TWSANetworkEvents = record
  lNetworkEvents: LongInt;
  iErrorCode: Array[0..FD_MAX_EVENTS-1] of Longint;
 end {TWSANetworkEvents};
 PWSANetworkEvents = ^TWSANetworkEvents;
 LPWSANetworkEvents = PWSANetworkEvents;

 TWSAProtocolChain = record
  ChainLen: Longint;     // the length of the chain,
                         // length = 0 means layered protocol,
                         // length = 1 means base protocol,
                         // length > 1 means protocol chain
  ChainEntries: Array[0..MAX_PROTOCOL_CHAIN-1] of LongInt; { a list of dwCatalogEntryIds }
 end {TWSAPROTOCOLCHAIN};
 
type
 TWSAProtocol_InfoA = record
  dwServiceFlags1: LongInt;
  dwServiceFlags2: LongInt;
  dwServiceFlags3: LongInt;
  dwServiceFlags4: LongInt;
  dwProviderFlags: LongInt;
  ProviderId: TGUID;
  dwCatalogEntryId: LongInt;
  ProtocolChain: TWSAProtocolChain;
  iVersion: Longint;
  iAddressFamily: Longint;
  iMaxSockAddr: Longint;
  iMinSockAddr: Longint;
  iSocketType: Longint;
  iProtocol: Longint;
  iProtocolMaxOffset: Longint;
  iNetworkByteOrder: Longint;
  iSecurityScheme: Longint;
  dwMessageSize: LongInt;
  dwProviderReserved: LongInt;
  szProtocol: Array[0..WSAPROTOCOL_LEN+1-1] of Char;
 end {TWSAProtocol_InfoA};
 PWSAProtocol_InfoA = ^TWSAProtocol_InfoA;
 LPWSAProtocol_InfoA = PWSAProtocol_InfoA;

 TWSAProtocol_InfoW = record
  dwServiceFlags1: LongInt;
  dwServiceFlags2: LongInt;
  dwServiceFlags3: LongInt;
  dwServiceFlags4: LongInt;
  dwProviderFlags: LongInt;
  ProviderId: TGUID;
  dwCatalogEntryId: LongInt;
  ProtocolChain: TWSAProtocolChain;
  iVersion: Longint;
  iAddressFamily: Longint;
  iMaxSockAddr: Longint;
  iMinSockAddr: Longint;
  iSocketType: Longint;
  iProtocol: Longint;
  iProtocolMaxOffset: Longint;
  iNetworkByteOrder: Longint;
  iSecurityScheme: Longint;
  dwMessageSize: LongInt;
  dwProviderReserved: LongInt;
  szProtocol: Array[0..(WSAPROTOCOL_LEN+1-1)] of WideChar;
 end {TWSAProtocol_InfoW};
 PWSAProtocol_InfoW = ^TWSAProtocol_InfoW;
 LPWSAProtocol_InfoW = PWSAProtocol_InfoW;

 TWSAProtocol_Info = TWSAProtocol_InfoA;
 LPWSAProtocol_Info = PWSAProtocol_InfoA;
 
type
{ SockAddr Information }
 SOCKET_ADDRESS = record
  lpSockaddr : PSockAddr;
  iSockaddrLength : Longint;
 end {SOCKET_ADDRESS};
 PSOCKET_ADDRESS = ^SOCKET_ADDRESS;

{ CSAddr Information }
 CSADDR_INFO = record
  LocalAddr, RemoteAddr: SOCKET_ADDRESS;
  iSocketType, iProtocol : LongInt;
 end {CSADDR_INFO};
 PCSADDR_INFO = ^CSADDR_INFO;

{ Address Family/Protocol Tuples }
 TAFProtocols = record
  iAddressFamily: Longint;
  iProtocol: Longint;
 end {AFPROTOCOLS};
 PAFProtocols = ^TAFProtocols;

{ Client Query API Typedefs }

{ The comparators }
 TWSAEComparator = (COMP_EQUAL {= 0}, COMP_NOTLESS );

 TWSAVersion = record
  dwVersion: LongInt;
  ecHow: TWSAEComparator;
 end {TWSAVersion};
 PWSAVersion = ^TWSAVersion;

 TWSAQuerySetA = record
  dwSize: LongInt;
  lpszServiceInstanceName: PChar;
  lpServiceClassId: PGUID;
  lpVersion: PWSAVERSION;
  lpszComment: PChar;
  dwNameSpace: LongInt;
  lpNSProviderId: PGUID;
  lpszContext: PChar;
  dwNumberOfProtocols: LongInt;
  lpafpProtocols: PAFProtocols;
  lpszQueryString: PChar;
  dwNumberOfCsAddrs: LongInt;
  lpcsaBuffer: PCSADDR_INFO;
  dwOutputFlags: LongInt;
  lpBlob: PBLOB;
 end {TWSAQuerySetA};
 PWSAQuerySetA = ^TWSAQuerySetA;
 LPWSAQuerySetA = PWSAQuerySetA;
 
 TWSAQuerySetW = record
  dwSize: LongInt;
  lpszServiceInstanceName: PWideChar;
  lpServiceClassId: PGUID;
  lpVersion: PWSAVERSION;
  lpszComment: PWideChar;
  dwNameSpace: LongInt;
  lpNSProviderId: PGUID;
  lpszContext: PWideChar;
  dwNumberOfProtocols: LongInt;
  lpafpProtocols: PAFProtocols;
  lpszQueryString: PWideChar;
  dwNumberOfCsAddrs: LongInt;
  lpcsaBuffer: PCSADDR_INFO;
  dwOutputFlags: LongInt;
  lpBlob: PBLOB;
 end {TWSAQuerySetW};
 PWSAQuerySetW = ^TWSAQuerySetW;
 LPWSAQuerySetW = PWSAQuerySetW;

 PWSAQuerySet = PWSAQuerySetA;
 LPWSAQuerySet = PWSAQuerySetA;

 PWSAMSG = ^TWSAMSG;
 TWSAMSG = record
  name: PSOCKET_ADDRESS;
  namelen: Longint;
  lpBuffers: LPWSABUF;
  dwBufferCount: DWORD;
  Control: WSABUF;
  dwFlags: DWORD;
 end;
 WSAMSG = TWSAMSG;
 LPWSAMSG = PWSAMSG;

type
{ Service Address Registration and Deregistration Data Types. }
 TWSAeSetServiceOp = (RNRSERVICE_REGISTER{=0},RNRSERVICE_DEREGISTER,RNRSERVICE_DELETE);

{ Service Installation/Removal Data Types. }
 TWSANSClassInfoA = record
  lpszName: PChar;
  dwNameSpace: LongInt;
  dwValueType: LongInt;
  dwValueSize: LongInt;
  lpValue: Pointer;
 end {_WSANSClassInfoA};
 PWSANSClassInfoA = ^TWSANSClassInfoA;
 
 TWSANSClassInfoW = record
  lpszName: PWideChar;
  dwNameSpace: LongInt;
  dwValueType: LongInt;
  dwValueSize: LongInt;
  lpValue: Pointer;
 end {TWSANSClassInfoW};
 PWSANSClassInfoW = ^TWSANSClassInfoW;

 TWSANSClassInfo = TWSANSClassInfoA;
 PWSANSClassInfo = PWSANSClassInfoA;
 LPWSANSClassInfo = PWSANSClassInfoA;

 TWSAServiceClassInfoA = record
  lpServiceClassId: PGUID;
  lpszServiceClassName: PChar;
  dwCount: LongInt;
  lpClassInfos: PWSANSClassInfoA;
 end {TWSAServiceClassInfoA};
 PWSAServiceClassInfoA = ^TWSAServiceClassInfoA;
 LPWSAServiceClassInfoA = PWSAServiceClassInfoA;

 TWSAServiceClassInfoW = record
  lpServiceClassId: PGUID;
  lpszServiceClassName: PWideChar;
  dwCount: LongInt;
  lpClassInfos: PWSANSClassInfoW;
 end {TWSAServiceClassInfoW};
 PWSAServiceClassInfoW = ^TWSAServiceClassInfoW;
 LPWSAServiceClassInfoW = PWSAServiceClassInfoW;

 TWSAServiceClassInfo = TWSAServiceClassInfoA;
 PWSAServiceClassInfo = PWSAServiceClassInfoA;
 LPWSAServiceClassInfo = PWSAServiceClassInfoA;

 TWSANameSpace_InfoA = record
  NSProviderId: TGUID;
  dwNameSpace: LongInt;
  fActive: LongInt{Bool};
  dwVersion: LongInt;
  lpszIdentifier: PChar;
 end {TWSANameSpace_InfoA};
 PWSANameSpace_InfoA = ^TWSANameSpace_InfoA;
 LPWSANameSpace_InfoA = PWSANameSpace_InfoA;

 TWSANameSpace_InfoW = record
  NSProviderId: TGUID;
  dwNameSpace: LongInt;
  fActive: LongInt{Bool};
  dwVersion: LongInt;
  lpszIdentifier: PWideChar;
 end {TWSANameSpace_InfoW};
 PWSANameSpace_InfoW = ^TWSANameSpace_InfoW;
 LPWSANameSpace_InfoW = PWSANameSpace_InfoW;

 TWSANameSpace_Info = TWSANameSpace_InfoA;
 PWSANameSpace_Info = PWSANameSpace_InfoA;
 LPWSANameSpace_Info = PWSANameSpace_InfoA;

type
{ WinSock 2 extensions -- data types for the condition function in }
{ WSAAccept() and overlapped I/O completion routine. }
 LPCONDITIONPROC = function (lpCallerId: LPWSABUF; lpCallerData : LPWSABUF; lpSQOS,lpGQOS : LPQOS; lpCalleeId,lpCalleeData : LPWSABUF; g : GROUP; dwCallbackData : DWORD ) : Longint;{$IFDEF i386} stdcall;{$ENDIF}
 LPWSAOVERLAPPED_COMPLETION_ROUTINE = procedure ( const dwError, cbTransferred : DWORD; const lpOverlapped : LPWSAOVERLAPPED; const dwFlags : DWORD );{$IFDEF i386} stdcall;{$ENDIF}
 
type
 {Structure used in getaddrinfo() call}
 PAddrInfo = GlobalSock.PAddrInfo;
 TAddrInfo = GlobalSock.TAddrInfo;
 
{==============================================================================}
type
 {Winsock2 specific classes}
 
 {Base Socket classes}
 TWinsock2Socket = class(TListObject)
 public
  {}
  constructor Create;
 private
  {}
  FBoundPort:Word;
  FBoundAddress:String;

  FReuseAddress:Boolean;
  {}
  procedure SetFamily(AFamily:Integer); virtual;
  procedure SetSocketType(ASocketType:Integer); virtual;
  procedure SetProtocol(AProtocol:Integer); virtual;

  procedure SetBoundPort(ABoundPort:Word);
  procedure SetBoundAddress(const ABoundAddress:String); virtual;

  procedure SetReuseAddress(AReuseAddress:Boolean);

  function GetLocalPort:Word;
  function GetLocalHost:String;
  function GetLocalAddress:String;
  function GetLocalAddresses:TStrings;
 
  function GetSendSize:Integer;
  procedure SetSendSize(ASize:Integer);
  function GetReceiveSize:Integer;
  procedure SetReceiveSize(ASize:Integer);
  function GetSendTimeout:Integer;
  procedure SetSendTimeout(ATimeout:Integer);
  function GetReceiveTimeout:Integer;
  procedure SetReceiveTimeout(ATimeout:Integer);
  function GetConnectTimeout:Integer;
  procedure SetConnectTimeout(ATimeout:Integer);
  //To Do //ExclusiveAddress (SO_EXCLUSIVEADDRUSE ) //other options //Linger / Error etc {See: getsockopt/setsockopt}
 protected
  {}
  FHandle:TSocket;

  FFamily:Integer;
  FSocketType:Integer;
  FProtocol:Integer;

  FLastError:LongInt;
  {}
  function GetBroadcastAddress:String;
  
  function AllocateFamily:Boolean; virtual;

  function AllocateAddress(var ALength:Integer):PSockAddr;
  function ReleaseAddress(var ASockAddr:PSockAddr;var ALength:Integer):Boolean;

  function AllocateBoundAddress(var ALength:Integer):PSockAddr;

  function AllocateSocket(ASocketType:Integer):TSocket;
  function Bind:LongInt;

  function SockAddrToPort(ASockAddr:PSockAddr;ALength:Integer):Word;
  function SockAddrToAddress(ASockAddr:PSockAddr;ALength:Integer):String;
  function PortToSockAddr(APort:Word;ASockAddr:PSockAddr;ALength:Integer):Boolean;
  function AddressToSockAddr(const AAddress:String;var ALength:Integer):PSockAddr;
 public
  {}
  property Handle:TSocket read FHandle;

  property Family:Integer read FFamily write SetFamily;
  property SocketType:Integer read FSocketType write SetSocketType;
  property Protocol:Integer read FProtocol write SetProtocol;

  property LastError:LongInt read FLastError;

  property BoundPort:Word read FBoundPort write SetBoundPort;
  property BoundAddress:String read FBoundAddress write SetBoundAddress;

  property ReuseAddress:Boolean read FReuseAddress write SetReuseAddress;

  property LocalPort:Word read GetLocalPort;
  property LocalHost:String read GetLocalHost;
  property LocalAddress:String read GetLocalAddress;
  property LocalAddresses:TStrings read GetLocalAddresses;

  property SendSize:Integer read GetSendSize write SetSendSize;
  property ReceiveSize:Integer read GetReceiveSize write SetReceiveSize;
  property SendTimeout:Integer read GetSendTimeout write SetSendTimeout;
  property ReceiveTimeout:Integer read GetReceiveTimeout write SetReceiveTimeout;
  property ConnectTimeout:Integer read GetConnectTimeout write SetConnectTimeout;
  //To Do //ExclusiveAddress (SO_EXCLUSIVEADDRUSE ) //other options //Linger / Error etc {See: getsockopt/setsockopt}
  {}
  function Connected:Boolean; virtual;

  function Shutdown:Boolean; virtual;
  function Disconnect:Boolean; virtual;
  function CloseSocket:Boolean; virtual;

  function ResolveHost(const AHost:String):String;
  function ResolveFamily(const AAddress:String):Integer;
  function ResolveAddress(const AAddress:String):String;

  function ResolveHostEx(const AHost:String;AFamily:Integer;AAll:Boolean):TStrings;
 end;
 
 TWinsock2SocketThread = class(TThreadEx) //To do //Base on TListObject instead ? //Move the stuff from ThreadEx to Here ?
 private
  {}
  FPrev:TWinsock2SocketThread;
  FNext:TWinsock2SocketThread;
 protected
  {}
 public
  {}
  property Prev:TWinsock2SocketThread read FPrev write FPrev;
  property Next:TWinsock2SocketThread read FNext write FNext;
 end;
 
 TWinsock2SocketThreads = class(TObject) //To do //Base on TThreadLinkedList instead when completed //Need to override Add/Remove/Insert (for Find) and add Find
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {}
  FLock:TCriticalSectionHandle; //To Do //Reader/Writer ? //For Performance ? //If Yes then do not base on TThreadLinkedList //Maybe create TWinsock2SocketList instead ?

  FCount:Integer;
  FFirst:TWinsock2SocketThread;
  FLast:TWinsock2SocketThread;

  function GetCount:Integer;
  function GetFirst:TWinsock2SocketThread;
  function GetLast:TWinsock2SocketThread;
  function Link(AValue:TWinsock2SocketThread):Boolean;
  function LinkEx(APrev,AValue:TWinsock2SocketThread):Boolean;
  function Unlink(AValue:TWinsock2SocketThread):Boolean;
 protected
  {}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
 public
  {}
  property Count:Integer read GetCount;
  property First:TWinsock2SocketThread read GetFirst;
  property Last:TWinsock2SocketThread read GetLast;
  function Add(AValue:TWinsock2SocketThread):Boolean; virtual;
  function Remove(AValue:TWinsock2SocketThread):Boolean; virtual;
  function Insert(APrev,AValue:TWinsock2SocketThread):Boolean; virtual;
  function Find(AValue:TWinsock2SocketThread):Boolean; virtual;
  function FindByID(AThreadID:TThreadID):TWinsock2SocketThread; virtual;
  procedure Clear; virtual;
 end;

 TWinsock2SocketBuffer = class(TObject) //To do //Base on TListObject instead ?
 private
  {}
  FPrev:TWinsock2SocketBuffer;
  FNext:TWinsock2SocketBuffer;
 protected
  {}
 public
  {}
  property Prev:TWinsock2SocketBuffer read FPrev write FPrev;
  property Next:TWinsock2SocketBuffer read FNext write FNext;
 end;
 
 TWinsock2SocketBuffers = class(TObject) //To do //Base on TThreadLinkedList instead when completed //Need to override Add/Remove/Insert (for Find) and add Find
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {}
  FLock:TCriticalSectionHandle; //To Do //Reader/Writer ? //For Performance ? //If Yes then do not base on TThreadLinkedList //Maybe create TWinsock2SocketList instead ?

  FCount:Integer;
  FFirst:TWinsock2SocketBuffer;
  FLast:TWinsock2SocketBuffer;

  function GetCount:Integer;
  function GetFirst:TWinsock2SocketBuffer;
  function GetLast:TWinsock2SocketBuffer;
  function Link(AValue:TWinsock2SocketBuffer):Boolean;
  function LinkEx(APrev,AValue:TWinsock2SocketBuffer):Boolean;
  function Unlink(AValue:TWinsock2SocketBuffer):Boolean;
 protected
  {}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
 public
  {}
  property Count:Integer read GetCount;
  property First:TWinsock2SocketBuffer read GetFirst;
  property Last:TWinsock2SocketBuffer read GetLast;
  function Add(AValue:TWinsock2SocketBuffer):Boolean; virtual;
  function Remove(AValue:TWinsock2SocketBuffer):Boolean; virtual;
  function Insert(APrev,AValue:TWinsock2SocketBuffer):Boolean; virtual;
  function Find(AValue:TWinsock2SocketBuffer):Boolean; virtual;
  procedure Clear; virtual;
 end;
 
 {Raw Socket (SOCK_RAW) classes}
 TWinsock2RAWSocket = class(TWinsock2Socket) 
 public
  {}
  constructor Create;
 private
  {}
  FBufferSize:Integer;
  FBroadcastEnabled:Boolean;
  
  procedure SetSocketType(ASocketType:Integer); override;
  procedure SetProtocol(AProtocol:Integer); override;

  procedure SetBufferSize(ABufferSize:Integer); 
  procedure SetBroadcastEnabled(ABroadcastEnabled:Boolean);
 protected
  {}
  function RecvFromSocket(ASockAddr:PSockAddr;ASockLen:PInteger;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt; virtual;
  function SendToSocket(ASockAddr:PSockAddr;ASockLen:Integer;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt; virtual;

  function RecvFromSocketEx(AHandle:THandle;ASockAddr:PSockAddr;ASockLen:PInteger;AData:Pointer;ASize:Integer;var ACount:Integer;var AClosed:Boolean):LongInt; virtual;
  function SendToSocketEx(AHandle:THandle;ASockAddr:PSockAddr;ASockLen:Integer;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt; virtual;
 public
  {}
  property BufferSize:Integer read FBufferSize write SetBufferSize;
  property BroadcastEnabled:Boolean read FBroadcastEnabled write SetBroadcastEnabled;
  {}
  function RecvData(AData:Pointer;ACount:Integer):Integer; virtual;
  function SendData(AData:Pointer;ACount:Integer):Integer; virtual;
  function BroadcastData(AData:Pointer;ACount:Integer):Integer; virtual;
  
  function RecvDataFrom(var AHost:String;AData:Pointer;ACount:Integer):Integer; virtual;
  function SendDataTo(const AHost:String;AData:Pointer;ACount:Integer):Integer; virtual;
  function BroadcastDataTo(const AAddress:String;AData:Pointer;ACount:Integer):Integer; virtual;
 end;
 
 {Stream Socket (SOCK_STREAM) classes}
 TWinsock2TCPSocket = class(TWinsock2Socket) 
 public
  {}
  constructor Create;
 private
  {}
  FUseNagle:Boolean;
  FUseKeepalive:Boolean;
  FSegmentSize:Integer;
  FMaxSegmentSize:LongWord;

  procedure SetSocketType(ASocketType:Integer); override;
  procedure SetProtocol(AProtocol:Integer); override;

  procedure SetBacklog(ABacklog:Integer);
  
  procedure SetUseNagle(AUseNagle:Boolean);
  procedure SetUseKeepalive(AUseKeepalive:Boolean);
  procedure SetSegmentSize(ASegmentSize:Integer);
  procedure SetMaxSegmentSize(AMaxSegmentSize:LongWord);
 protected
  {}
  FBacklog:Integer;
  
  {}  
  function Listen(ABacklog:Integer):LongInt;
  
  function ReadFromSocket(AData:Pointer;ASize:Integer):LongInt;
  function WriteToSocket(AData:Pointer;ASize:Integer):LongInt;

  function ReadFromSocketEx(AData:Pointer;ASize:Integer;var ACount:Integer;var AClosed:Boolean;AWait:Boolean;ATimeout:Integer):LongInt;
  function WriteToSocketEx(AData:Pointer;ASize:Integer;var ACount:Integer;AWait:Boolean;ATimeout:Integer):LongInt;
 public
  {}
  property Backlog:Integer read FBacklog write SetBacklog;

  property UseNagle:Boolean read FUseNagle write SetUseNagle;
  property UseKeepalive:Boolean read FUseKeepalive write SetUseKeepalive;
  property SegmentSize:Integer read FSegmentSize write SetSegmentSize;
  property MaxSegmentSize:LongWord read FMaxSegmentSize write SetMaxSegmentSize;
  
  {}
  function ReadData(AData:Pointer;ACount:Integer):Boolean;
  function WriteData(AData:Pointer;ACount:Integer):Boolean;

  function ReadAvailable(AData:Pointer;ASize:Integer;var ACount:Integer;var AClosed:Boolean):Boolean;
 end;

 {Datagram Socket (SOCK_DGRAM) classes}
 TWinsock2UDPSocket = class(TWinsock2Socket) 
 public
  {}
  constructor Create;
 private
  {}
  FBufferSize:Integer;
  FBroadcastEnabled:Boolean;
  
  procedure SetSocketType(ASocketType:Integer); override;
  procedure SetProtocol(AProtocol:Integer); override;
  
  procedure SetBufferSize(ABufferSize:Integer); 
  procedure SetBroadcastEnabled(ABroadcastEnabled:Boolean);
 protected
  {}
  function RecvFromSocket(ASockAddr:PSockAddr;ASockLen:PInteger;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt; virtual;
  function SendToSocket(ASockAddr:PSockAddr;ASockLen:Integer;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt; virtual;

  function RecvFromSocketEx(AHandle:THandle;ASockAddr:PSockAddr;ASockLen:PInteger;AData:Pointer;ASize:Integer;var ACount:Integer;var AClosed:Boolean):LongInt; virtual;
  function SendToSocketEx(AHandle:THandle;ASockAddr:PSockAddr;ASockLen:Integer;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt; virtual;
 public
  {}
  property BufferSize:Integer read FBufferSize write SetBufferSize;
  property BroadcastEnabled:Boolean read FBroadcastEnabled write SetBroadcastEnabled;
  {}
  function RecvData(AData:Pointer;ACount:Integer):Integer; virtual;
  function SendData(AData:Pointer;ACount:Integer):Integer; virtual;
  function BroadcastData(APort:Word;AData:Pointer;ACount:Integer):Integer; virtual;

  function RecvDataFrom(var AHost:String;var APort:Word;AData:Pointer;ACount:Integer):Integer; virtual;
  function SendDataTo(const AHost:String;APort:Word;AData:Pointer;ACount:Integer):Integer; virtual;
  function BroadcastDataTo(const AAddress:String;APort:Word;AData:Pointer;ACount:Integer):Integer; virtual;
 end;

 {Raw Client classes}
 TWinsock2RAWClient = class(TWinsock2RAWSocket)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {}
  FUseConnect:Boolean;

  FRemoteHost:String;
  FRemoteAddress:String;

  {}
  procedure SetUseConnect(AUseConnect:Boolean);

  procedure SetFamily(AFamily:Integer); override;

  procedure SetBoundAddress(const ABoundAddress:String); override;
  
  procedure SetRemoteHost(const ARemoteHost:String);
  procedure SetRemoteAddress(const ARemoteAddress:String);
 protected
  {}
  function AllocateFamily:Boolean; override;

  function AllocateRemoteAddress(var ALength:Integer):PSockAddr;
 public
  {}
  property UseConnect:Boolean read FUseConnect write SetUseConnect;

  property RemoteHost:String read FRemoteHost write SetRemoteHost;
  property RemoteAddress:String read FRemoteAddress write SetRemoteAddress;

  {}
  function Connect:Boolean; virtual;
  function Disconnect:Boolean; override;
 end;
 
 {Stream Client classes}
 TWinsock2TCPClient = class(TWinsock2TCPSocket)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {}
  FRemotePort:Word;
  FRemoteHost:String;
  FRemoteAddress:String;

  {}
  procedure SetFamily(AFamily:Integer); override;

  procedure SetBoundAddress(const ABoundAddress:String); override;

  procedure SetRemotePort(ARemotePort:Word);
  procedure SetRemoteHost(const ARemoteHost:String);
  procedure SetRemoteAddress(const ARemoteAddress:String);
 protected
  {}
  function AllocateFamily:Boolean; override;

  function AllocateRemoteAddress(var ALength:Integer):PSockAddr;
 public
  {}
  property RemotePort:Word read FRemotePort write SetRemotePort;
  property RemoteHost:String read FRemoteHost write SetRemoteHost;
  property RemoteAddress:String read FRemoteAddress write SetRemoteAddress;
  
  {}
  function Connect:Boolean; virtual;
  function ConnectEx:Boolean; virtual;
  function Disconnect:Boolean; override;
 end;
 
 {Datagram Client classes}
 TWinsock2UDPClient = class(TWinsock2UDPSocket)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {}
  FUseConnect:Boolean;

  FRemotePort:Word;
  FRemoteHost:String;
  FRemoteAddress:String;
  
  {}
  procedure SetUseConnect(AUseConnect:Boolean);

  procedure SetFamily(AFamily:Integer); override;

  procedure SetBoundAddress(const ABoundAddress:String); override;
  
  procedure SetRemotePort(ARemotePort:Word);
  procedure SetRemoteHost(const ARemoteHost:String);
  procedure SetRemoteAddress(const ARemoteAddress:String);
 protected
  {}
  function AllocateFamily:Boolean; override;

  function AllocateRemoteAddress(var ALength:Integer):PSockAddr;
 public
  {}
  property UseConnect:Boolean read FUseConnect write SetUseConnect;

  property RemotePort:Word read FRemotePort write SetRemotePort;
  property RemoteHost:String read FRemoteHost write SetRemoteHost;
  property RemoteAddress:String read FRemoteAddress write SetRemoteAddress;
  
  {}
  function Connect:Boolean; virtual;
  function Disconnect:Boolean; override;
 end;
 
 {Stream Server classes}
 TWinsock2TCPListener = class;
 TWinsock2TCPServer = class(TWinsock2TCPSocket)
 public
  {}
  constructor Create(AListener:TWinsock2TCPListener);
 private
  {}
  FPeerPort:Word;
  FPeerAddress:String;

  FListener:TWinsock2TCPListener;
 protected
  {}
  procedure SetHandle(AHandle:TSocket); virtual;
  procedure SetLastError(ALastError:LongInt); virtual;
 public
  {}
  property PeerPort:Word read FPeerPort;
  property PeerAddress:String read FPeerAddress;
  property Listener:TWinsock2TCPListener read FListener;

  function Disconnect:Boolean; override;
 end;

 TWinsock2TCPServerThread = class(TWinsock2SocketThread)
 public
  {}
  constructor Create(AServer:TWinsock2TCPServer);
  destructor Destroy; override;
 private
  {}
  FData:TObject;
  FServer:TWinsock2TCPServer;
 protected
  {}
  procedure AfterExecution; override;
  procedure BeforeExecution; override;
 public
  {}
  property Data:TObject read FData write FData;
  property Server:TWinsock2TCPServer read FServer;

  procedure Execution; override;
 end;

 TWinsock2TCPListenerThread = class(TWinsock2SocketThread)
 public
  {}
  constructor Create(AListener:TWinsock2TCPListener);
 private
  {}
  FListener:TWinsock2TCPListener;
 protected
  {}
  procedure AfterExecution; override;
  procedure BeforeExecution; override;
 public
  {}
  property Listener:TWinsock2TCPListener read FListener;

  procedure Execution; override;
 end;

 TWinsock2TCPServerThreads = class(TWinsock2SocketThreads)
 private
  {}
  procedure TerminateThread(AThread:TWinsock2TCPServerThread);
 protected
  {}
 public
  {}
  procedure TerminateAll;
  function Terminate(AThread:TWinsock2TCPServerThread):Boolean;
 end;

 TTCPExecuteEvent = function(AThread:TWinsock2TCPServerThread):Boolean of Object;
 TTCPConnectEvent = procedure(AThread:TWinsock2TCPServerThread) of Object;
 TTCPDisconnectEvent = procedure(AThread:TWinsock2TCPServerThread) of Object;
 TTCPCreateThreadEvent = procedure(AServer:TWinsock2TCPServer;var AThread:TWinsock2TCPServerThread) of Object;
 TTCPSelectThreadEvent = procedure(AServer:TWinsock2TCPServer;var AThread:TWinsock2TCPServerThread) of Object;

 TWinsock2TCPListener = class(TWinsock2TCPSocket)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {}
  FActive:Boolean;
  FThreads:TWinsock2TCPServerThreads;
  FListenerThread:TWinsock2TCPListenerThread;

  FOnExecute:TTCPExecuteEvent;
  FOnConnect:TTCPConnectEvent;
  FOnDisconnect:TTCPDisconnectEvent;
  FOnCreateThread:TTCPCreateThreadEvent;
  {}
  procedure SetActive(AActive:Boolean);
 protected
  {}
  procedure SetLastError(ALastError:LongInt); virtual;

  procedure DoConnect(AThread:TWinsock2TCPServerThread); virtual;
  procedure DoDisconnect(AThread:TWinsock2TCPServerThread); virtual;
  
  function DoExecute(AThread:TWinsock2TCPServerThread):Boolean; virtual;
 public
  {}
  property Active:Boolean read FActive write SetActive;
  property Threads:TWinsock2TCPServerThreads read FThreads;

  property OnExecute:TTCPExecuteEvent read FOnExecute write FOnExecute;
  property OnConnect:TTCPConnectEvent read FOnConnect write FOnConnect;
  property OnDisconnect:TTCPDisconnectEvent read FOnDisconnect write FOnDisconnect;
  property OnCreateThread:TTCPCreateThreadEvent read FOnCreateThread write FOnCreateThread;
 end;
 
 {Datagrams Server classes}
 TWinsock2UDPListener = class;
 TWinsock2UDPServerBuffer = class;
 TWinsock2UDPServer = class(TWinsock2UDPSocket)
 public
  {}
  constructor Create(AListener:TWinsock2UDPListener);
  destructor Destroy; override;
 private
  {}
  FPeerPort:Word;
  FPeerAddress:String;

  FUseListener:Boolean;
  FListener:TWinsock2UDPListener;
  {}
  procedure SetUseListener(AUseListener:Boolean);
 protected
  {}
  FBuffer:TWinsock2UDPServerBuffer;
  
  function GetData:Pointer; virtual;
  function GetSize:Integer; virtual;
  function GetCount:Integer; virtual;
  
  procedure SetBuffer(ABuffer:TWinsock2UDPServerBuffer); virtual;
  procedure SetLastError(ALastError:LongInt); virtual;
 public
  {}
  property PeerPort:Word read FPeerPort;
  property PeerAddress:String read FPeerAddress;
  property UseListener:Boolean read FUseListener write SetUseListener;
  property Listener:TWinsock2UDPListener read FListener;
  
  property Buffer:TWinsock2UDPServerBuffer read FBuffer;
  
  property Data:Pointer read GetData;
  property Size:Integer read GetSize;
  property Count:Integer read GetCount;
  
  function RecvData(AData:Pointer;ACount:Integer):Integer; override;
  function SendData(AData:Pointer;ACount:Integer):Integer; override;
  function BroadcastData(APort:Word;AData:Pointer;ACount:Integer):Integer; override;
  
  function RecvDataFrom(var AHost:String;var APort:Word;AData:Pointer;ACount:Integer):Integer; override;
  function SendDataTo(const AHost:String;APort:Word;AData:Pointer;ACount:Integer):Integer; override;
  function BroadcastDataTo(const AAddress:String;APort:Word;AData:Pointer;ACount:Integer):Integer; override;
 end;
 
 TWinsock2UDPServerThread = class(TWinsock2SocketThread)
 public
  {}
  constructor Create(AServer:TWinsock2UDPServer);
  destructor Destroy; override;
 private
  {}
  FLock:TCriticalSectionHandle;
  
  FActive:Boolean;
  FData:TObject;
  FServer:TWinsock2UDPServer;
  {}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
  
  function GetActive:Boolean;
  procedure SetActive(AActive:Boolean);
 protected
  {}
  procedure AfterExecution; override;
  procedure BeforeExecution; override;
 public
  {}
  property Active:Boolean read GetActive write SetActive;
  property Data:TObject read FData write FData;
  property Server:TWinsock2UDPServer read FServer;
  {}
  procedure Execution; override;
 end;
 
 TWinsock2UDPListenerThread = class(TWinsock2SocketThread)
 public
  {}
  constructor Create(AListener:TWinsock2UDPListener);
  destructor Destroy; override;
 private
  {}
  FListener:TWinsock2UDPListener;
 protected
  {}
  procedure AfterExecution; override;
  procedure BeforeExecution; override;
 public
  {}
  property Listener:TWinsock2UDPListener read FListener;
  {}
  procedure Execution; override;
 end;
 
 TWinsock2UDPServerThreads = class(TWinsock2SocketThreads)
 public
  {}
  constructor Create(AListener:TWinsock2UDPListener);
 private
  {}
  FMin:Integer;
  FMax:Integer;

  FListener:TWinsock2UDPListener;
  {}
  function GetMin:Integer;
  procedure SetMin(AMin:Integer);
  function GetMax:Integer;
  procedure SetMax(AMax:Integer);
   
  procedure CreateThreads;
  function CreateThread(AForce:Boolean):TWinsock2UDPServerThread;
  procedure TerminateThread(AThread:TWinsock2UDPServerThread);
 public
  {}
  property Min:Integer read GetMin write SetMin;
  property Max:Integer read GetMax write SetMax;
  {}
  function GetThread:TWinsock2UDPServerThread;
  procedure ReleaseThread(AThread:TWinsock2UDPServerThread);

  procedure TerminateAll;
  function Terminate(AThread:TWinsock2UDPServerThread):Boolean;
 end;
 
 TWinsock2UDPServerBuffer = class(TWinsock2SocketBuffer)
 public
  {}
  constructor Create(ASize:Integer);
  destructor Destroy; override;
 private
  {}
  FLock:TCriticalSectionHandle;
  
  FActive:Boolean;
  FData:Pointer;
  FSize:Integer;
  FCount:Integer;
  {}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
  
  function GetActive:Boolean;
  procedure SetActive(AActive:Boolean);
  procedure SetCount(ACount:Integer);
 public
  {}
  property Active:Boolean read GetActive write SetActive;
  property Data:Pointer read FData;
  property Size:Integer read FSize;
  property Count:Integer read FCount write SetCount;
 end;
 
 TWinsock2UDPServerBuffers = class(TWinsock2SocketBuffers)
 public
  {}
  constructor Create(AListener:TWinsock2UDPListener);
 private
  {}
  FMin:Integer;
  FMax:Integer;

  FListener:TWinsock2UDPListener;
  {}
  function GetMin:Integer;
  procedure SetMin(AMin:Integer);
  function GetMax:Integer;
  procedure SetMax(AMax:Integer);
   
  procedure CreateBuffers;
  function CreateBuffer(AForce:Boolean):TWinsock2UDPServerBuffer;
  procedure DeleteBuffer(ABuffer:TWinsock2UDPServerBuffer);
 public
  {}
  property Min:Integer read GetMin write SetMin;
  property Max:Integer read GetMax write SetMax;
  {}
  function GetBuffer:TWinsock2UDPServerBuffer;
  procedure ReleaseBuffer(ABuffer:TWinsock2UDPServerBuffer);

  procedure DeleteAll;
  function Delete(ABuffer:TWinsock2UDPServerBuffer):Boolean;
 end;
 
 TUDPExecuteEvent = function(AThread:TWinsock2UDPServerThread):Boolean of Object;
 TUDPCreateThreadEvent = procedure(AServer:TWinsock2UDPServer;var AThread:TWinsock2UDPServerThread) of Object;
 TUDPSelectThreadEvent = procedure(AServer:TWinsock2UDPServer;var AThread:TWinsock2UDPServerThread) of Object;
 TUDPCreateBufferEvent = procedure(ASize:Integer;var ABuffer:TWinsock2UDPServerBuffer) of Object;
 TUDPSelectBufferEvent = procedure(ASize:Integer;var ABuffer:TWinsock2UDPServerBuffer) of Object;
 
 TWinsock2UDPListener = class(TWinsock2UDPSocket)
 public
  {}
  constructor Create;
  destructor Destroy; override;
 private
  {}
  FLock:TCriticalSectionHandle;

  FActive:Boolean;
  FUseListener:Boolean;
  FThreads:TWinsock2UDPServerThreads;
  FBuffers:TWinsock2UDPServerBuffers;
  FListenerThread:TWinsock2UDPListenerThread;

  FOnExecute:TUDPExecuteEvent;
  FOnCreateThread:TUDPCreateThreadEvent;
  FOnCreateBuffer:TUDPCreateBufferEvent;
  {}
  function AcquireLock:Boolean;
  function ReleaseLock:Boolean;
  
  procedure SetActive(AActive:Boolean);
  procedure SetUseListener(AUseListener:Boolean);
 protected
  {}
  procedure SetLastError(ALastError:LongInt); virtual;

  function DoExecute(AThread:TWinsock2UDPServerThread):Boolean; virtual;
  
  function SendToSocketEx(AHandle:THandle;ASockAddr:PSockAddr;ASockLen:Integer;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt; override;
 public
  {}
  property Active:Boolean read FActive write SetActive;
  property UseListener:Boolean read FUseListener write SetUseListener;
  property Threads:TWinsock2UDPServerThreads read FThreads;
  property Buffers:TWinsock2UDPServerBuffers read FBuffers;
  
  property OnExecute:TUDPExecuteEvent read FOnExecute write FOnExecute;
  property OnCreateThread:TUDPCreateThreadEvent read FOnCreateThread write FOnCreateThread;
  property OnCreateBuffer:TUDPCreateBufferEvent read FOnCreateBuffer write FOnCreateBuffer;
 end;
 
{==============================================================================}
{var}
 {Winsock2 specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure WS2Init;
function WS2Start:Boolean;
function WS2Stop:Boolean;

procedure WS2AsyncStart(Data:Pointer);

{==============================================================================}
{Winsock2 Functions}
function accept( const s: TSocket; addr: PSockAddr; addrlen: PLongint ): TSocket; overload;
function accept( const s: TSocket; addr: PSockAddr; var addrlen: Longint ): TSocket; overload;
function bind( const s: TSocket; addr: PSockAddr; namelen: Longint ): Longint; overload;
function bind( const s: TSocket; var addr: TSockAddr; namelen: Longint ): Longint; overload;
function closesocket( const s: TSocket ): Longint; 
function connect( const s: TSocket; name: PSockAddr; namelen: Longint): Longint; overload;
function connect( const s: TSocket; var name: TSockAddr; namelen: Longint): Longint; overload;
function ioctlsocket( const s: TSocket; cmd: Longint; var arg: u_long ): Longint; overload;
function ioctlsocket( const s: TSocket; cmd: Longint; argp: pu_long ): Longint; overload;
function getpeername( const s: TSocket; var name: TSockAddr; var namelen: Longint ): Longint; 
function getsockname( const s: TSocket; var name: TSockAddr; var namelen: Longint ): Longint; 
function getsockopt( const s: TSocket; const level, optname: Longint; optval: PChar; var optlen: Longint ): Longint; overload;
function getsockopt( const s: TSocket; const level, optname: Longint; optval: Pointer; var optlen: Longint ): Longint; overload;
function getsockopt( const s: TSocket; const level, optname: Longint; var optval; var optlen: Longint ): Longint; overload;
function htonl(hostlong: u_long): u_long;
function htons(hostshort: u_short): u_short;
function inet_addr(cp: PChar): u_long;
function inet_ntoa(inaddr: TInAddr): PChar; 
function listen(s: TSocket; backlog: Longint): Longint; 
function ntohl(netlong: u_long): u_long;
function ntohs(netshort: u_short): u_short; 
function recv(s: TSocket; var Buf; len, flags: Longint): Longint;  overload;
function recv(s: TSocket; Buf: PChar; len, flags: Longint): Longint; overload;
function recv(s: TSocket; Buf: Pointer; len, flags: Longint): Longint;  overload;
function recvfrom(s: TSocket; Buf: PChar; len, flags: Longint; from: PSockAddr; fromlen: PLongint): Longint; overload;
function recvfrom(s: TSocket; Buf: Pointer; len, flags: Longint; from: PSockAddr; fromlen: PLongint): Longint; overload;
function recvfrom(s: TSocket; var Buf; len, flags: Longint; var from: TSockAddr; var fromlen: Longint): Longint; overload;
function select(nfds: Longint; readfds, writefds, exceptfds: PFDSet; timeout: PTimeVal): Longint; 
function send(s: TSocket; var Buf; len, flags: Longint): Longint; overload;
function send(s: TSocket; Buf: PChar; len, flags: Longint): Longint; overload;
function send(s: TSocket; Buf: Pointer; len, flags: Longint): Longint; overload;
function sendto(s: TSocket; var Buf; len, flags: Longint; var addrto: TSockAddr; tolen: Longint): Longint; overload;
function sendto(s: TSocket; Buf: PChar; len, flags: Longint; addrto: PSockAddr; tolen: Longint): Longint; overload;
function sendto(s: TSocket; Buf: Pointer; len, flags: Longint; addrto: PSockAddr; tolen: Longint): Longint; overload;
function setsockopt(s: TSocket; level, optname: Longint; const optval; optlen: Longint): Longint; overload;
function setsockopt(s: TSocket; level, optname: Longint; optval: PChar; optlen: Longint): Longint; overload;
function setsockopt(s: TSocket; level, optname: Longint; optval: Pointer; optlen: Longint): Longint; overload;
function shutdown(s: TSocket; how: Longint): Longint; 
function socket(af, struct, protocol: Longint): TSocket; 

function gethostbyaddr(addr: Pointer; len, family: Longint): PHostEnt; 
function gethostbyname(name: PChar): PHostEnt; 
function gethostname(name: PChar; len: Longint): Longint; 
function getservbyport(port: Longint; proto: PChar): PServEnt; 
function getservbyname(name, proto: PChar): PServEnt; 
function getprotobynumber(proto: Longint): PProtoEnt; 
function getprotobyname(name: PChar): PProtoEnt; 

function getaddrinfo(pNodeName, pServiceName: PChar; pHints: PAddrInfo; var ppResult: PAddrInfo): LongInt;
procedure freeaddrinfo(ai: PAddrInfo);
function getnameinfo(sa: PSockAddr; salen: Integer; host: PChar; hostlen: DWORD; serv: PChar; servlen: DWORD; flags: Integer): Integer;

function WSAStartup(wVersionRequired: word; var WSData: TWSAData): Longint;
function WSACleanup: Longint; 
procedure WSASetLastError(iError: Longint); inline;
function WSAGetLastError: Longint; inline;
function WSAIsBlocking: BOOL;
function WSAUnhookBlockingHook: Longint; 
function WSASetBlockingHook(lpBlockFunc: TFarProc): TFarProc;
function WSACancelBlockingCall: Longint;
function WSAAsyncGetServByName(HWindow: HWND; wMsg: u_int; name, proto, buf: PChar; buflen: Longint): THandle;
function WSAAsyncGetServByPort( HWindow: HWND; wMsg, port: u_int; proto, buf: PChar; buflen: Longint): THandle;
function WSAAsyncGetProtoByName(HWindow: HWND; wMsg: u_int; name, buf: PChar; buflen: Longint): THandle;
function WSAAsyncGetProtoByNumber(HWindow: HWND; wMsg: u_int; number: Longint; buf: PChar; buflen: Longint): THandle;
function WSAAsyncGetHostByName(HWindow: HWND; wMsg: u_int; name, buf: PChar; buflen: Longint): THandle;
function WSAAsyncGetHostByAddr(HWindow: HWND; wMsg: u_int; addr: PChar; len, family: Longint; buf: PChar; buflen: Longint): THandle;
function WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Longint;
function WSAAsyncSelect(s: TSocket; HWindow: HWND; wMsg: u_int; lEvent: Longint): Longint;
function __WSAFDIsSet(s: TSOcket; var FDSet: TFDSet): Bool;

{ WinSock 2 API new function prototypes }
function inet_pton(Family: Longint; pszAddrString: PChar; pAddrBuf: Pointer): Longint;
function InetPtonA(Family: Longint; pszAddrString: PChar; pAddrBuf: Pointer): Longint;
function InetPtonW(Family: Longint; pszAddrString: PWideChar; pAddrBuf: Pointer): Longint;

function inet_ntop(Family: Longint; pAddr: Pointer; pStringBuf: PChar; StringBufSize: Longint): PChar;
function InetNtopA(Family: Longint; pAddr: Pointer; pStringBuf: PChar; StringBufSize: Longint): PChar;
function InetNtopW(Family: Longint; pAddr: Pointer; pStringBuf: PWideChar; StringBufSize: Longint): PWideChar;

function WSAAccept( s : TSocket; addr : TSockAddr; addrlen : PLongint; lpfnCondition : LPCONDITIONPROC; dwCallbackData : DWORD ): TSocket;
function WSACloseEvent( hEvent : WSAEVENT) : WordBool;
function WSAConnect( s : TSocket; const name : PSockAddr; namelen : Longint; lpCallerData,lpCalleeData : LPWSABUF; lpSQOS,lpGQOS : LPQOS ) : Longint;
function WSACreateEvent : WSAEVENT; 
function WSADuplicateSocketA( s : TSocket; dwProcessId : DWORD; lpProtocolInfo : LPWSAProtocol_InfoA ) : Longint;
function WSADuplicateSocketW( s : TSocket; dwProcessId : DWORD; lpProtocolInfo : LPWSAProtocol_InfoW ) : Longint;
function WSAEnumNetworkEvents( const s : TSocket; const hEventObject : WSAEVENT; lpNetworkEvents : LPWSANETWORKEVENTS ) :Longint;
function WSAEnumProtocolsA( lpiProtocols : PLongint; lpProtocolBuffer : LPWSAProtocol_InfoA; var lpdwBufferLength : DWORD ) : Longint;
function WSAEnumProtocolsW( lpiProtocols : PLongint; lpProtocolBuffer : LPWSAProtocol_InfoW; var lpdwBufferLength : DWORD ) : Longint;
function WSAEventSelect( s : TSocket; hEventObject : WSAEVENT; lNetworkEvents : LongInt ): Longint;
function WSAGetOverlappedResult( s : TSocket; lpOverlapped : LPWSAOVERLAPPED; lpcbTransfer : LPDWORD; fWait : BOOL; var lpdwFlags : DWORD ) : WordBool; 
function WSAGetQosByName( s : TSocket; lpQOSName : LPWSABUF; lpQOS : LPQOS ): WordBool;
function WSAhtonl( s : TSocket; hostlong : u_long; var lpnetlong : DWORD ): Longint; 
function WSAhtons( s : TSocket; hostshort : u_short; var lpnetshort : WORD ): Longint;
function WSAIoctl( s : TSocket; dwIoControlCode : DWORD; lpvInBuffer : Pointer; cbInBuffer : DWORD; lpvOutBuffer : Pointer; cbOutBuffer : DWORD; lpcbBytesReturned : LPDWORD; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE ) : Longint;
function WSAJoinLeaf( s : TSocket; name : PSockAddr; namelen : Longint; lpCallerData,lpCalleeData : LPWSABUF; lpSQOS,lpGQOS : LPQOS; dwFlags : DWORD ) : TSocket;
function WSANtohl( s : TSocket; netlong : u_long; var lphostlong : DWORD ): Longint;
function WSANtohs( s : TSocket; netshort : u_short; var lphostshort : WORD ): Longint;
function WSARecv( s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesRecvd : DWORD; var lpFlags : DWORD; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE ): Longint;
function WSARecvDisconnect( s : TSocket; lpInboundDisconnectData : LPWSABUF ): Longint;
function WSARecvFrom( s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesRecvd : DWORD; var lpFlags : DWORD; lpFrom : PSockAddr; lpFromlen : PLongint; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE ): Longint;
function WSARecvMsg( s : TSocket; lpMsg : LPWSAMSG; lpdwNumberOfBytesRecvd : DWORD; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE) : Longint;
function WSAResetEvent( hEvent : WSAEVENT ): WordBool;
function WSASend( s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesSent : DWORD; dwFlags : DWORD; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE ): Longint;
function WSASendDisconnect( s : TSocket; lpOutboundDisconnectData : LPWSABUF ): Longint;
function WSASendTo( s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesSent : DWORD; dwFlags : DWORD; lpTo : PSockAddr; iTolen : Longint; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE ): Longint;
function WSASendMsg( s : TSocket; lpMsg : LPWSAMSG; dwFlags : DWORD; lpNumberOfBytesSent : DWORD; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE) : Longint;
function WSASetEvent( hEvent : WSAEVENT ): WordBool;
function WSASocketA( af, iType, protocol : Longint; lpProtocolInfo : LPWSAProtocol_InfoA; g : GROUP; dwFlags : DWORD ): TSocket;
function WSASocketW( af, iType, protocol : Longint; lpProtocolInfo : LPWSAProtocol_InfoW; g : GROUP; dwFlags : DWORD ): TSocket;

function WSAWaitForMultipleEvents( cEvents : DWORD; lphEvents : PWSAEVENT; fWaitAll : LongBool; dwTimeout : DWORD; fAlertable : LongBool ): DWORD;
function WSAAddressToStringA( var lpsaAddress : TSockAddr; const dwAddressLength : DWORD; const lpProtocolInfo : LPWSAProtocol_InfoA; const lpszAddressString : PChar; var lpdwAddressStringLength : DWORD ): Longint;
function WSAAddressToStringW( var lpsaAddress : TSockAddr; const dwAddressLength : DWORD; const lpProtocolInfo : LPWSAProtocol_InfoW; const lpszAddressString : PWideChar; var lpdwAddressStringLength : DWORD ): Longint; 

function WSAStringToAddressA( const AddressString : PChar; const AddressFamily: Longint; const lpProtocolInfo : LPWSAProtocol_InfoA; var lpAddress : TSockAddr; var lpAddressLength : Longint ): Longint; 
function WSAStringToAddressW( const AddressString : PWideChar; const AddressFamily: Longint; const lpProtocolInfo : LPWSAProtocol_InfoA; var lpAddress : TSockAddr; var lpAddressLength : Longint ): Longint; 

{ Registration and Name Resolution API functions }
function WSALookupServiceBeginA( const lpqsRestrictions : LPWSAQuerySetA; const dwControlFlags : DWORD; lphLookup : PHANDLE ): Longint;
function WSALookupServiceBeginW( const lpqsRestrictions : LPWSAQuerySetW; const dwControlFlags : DWORD; lphLookup : PHANDLE ): Longint;

function WSALookupServiceNextA( const hLookup : THandle; const dwControlFlags : DWORD; var lpdwBufferLength : DWORD; lpqsResults : LPWSAQuerySetA ): Longint;
function WSALookupServiceNextW( const hLookup : THandle; const dwControlFlags : DWORD; var lpdwBufferLength : DWORD; lpqsResults : LPWSAQuerySetW ): Longint;
function WSALookupServiceEnd( const hLookup : THandle ): Longint;
function WSAInstallServiceClassA( const lpServiceClassInfo : LPWSAServiceClassInfoA ) : Longint;
function WSAInstallServiceClassW( const lpServiceClassInfo : LPWSAServiceClassInfoW ) : Longint;
function WSARemoveServiceClass( const lpServiceClassId : PGUID ) : Longint;
function WSAGetServiceClassInfoA( const lpProviderId : PGUID; const lpServiceClassId : PGUID; var lpdwBufSize : DWORD; lpServiceClassInfo : LPWSAServiceClassInfoA ): Longint;
function WSAGetServiceClassInfoW( const lpProviderId : PGUID; const lpServiceClassId : PGUID; var lpdwBufSize : DWORD; lpServiceClassInfo : LPWSAServiceClassInfoW ): Longint;

function WSAEnumNameSpaceProvidersA( var lpdwBufferLength: DWORD; const lpnspBuffer: LPWSANameSpace_InfoA ): Longint; 
function WSAEnumNameSpaceProvidersW( var lpdwBufferLength: DWORD; const lpnspBuffer: LPWSANameSpace_InfoW ): Longint; 

function WSAGetServiceClassNameByClassIdA( const lpServiceClassId: PGUID; lpszServiceClassName: PChar; var lpdwBufferLength: DWORD ): Longint;
function WSAGetServiceClassNameByClassIdW( const lpServiceClassId: PGUID; lpszServiceClassName: PWideChar; var lpdwBufferLength: DWORD ): Longint; 
function WSASetServiceA( const lpqsRegInfo: LPWSAQuerySetA; const essoperation: TWSAeSetServiceOp; const dwControlFlags: DWORD ): Longint; 
function WSASetServiceW( const lpqsRegInfo: LPWSAQuerySetW; const essoperation: TWSAeSetServiceOp; const dwControlFlags: DWORD ): Longint;

function WSAMakeSyncReply(Buflen, Error: Word): Longint;
function WSAMakeSelectReply(Event, Error: Word): Longint;
function WSAGetAsyncBuflen(Param: Longint): Word;
function WSAGetAsyncError(Param: Longint): Word;
function WSAGetSelectEvent(Param: Longint): Word;
function WSAGetSelectError(Param: Longint): Word;

procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
procedure FD_ZERO(var FDSet: TFDSet);

{==============================================================================}
{Winsock2 Undocumented Functions}
function WsControl(Proto:DWORD;Action:DWORD;pRequestInfo:Pointer; var pcbRequestInfoLen:DWORD;pResponseInfo:Pointer; var pcbResponseInfoLen:DWORD):Integer; 

function getnetbyaddr(addr: Pointer; len, Struct: Integer): PNetEnt; 
function getnetbyname(name: PChar): PNetEnt; 

{==============================================================================}
{Winsock2 Enhanced Functions}
function WsControlEx(Proto:DWORD;Action:DWORD;pRequestInfo:Pointer; var pcbRequestInfoLen:DWORD;pResponseInfo:Pointer; var pcbResponseInfoLen:DWORD):Integer; 

{==============================================================================}
{RTL Text IO Functions}
function SysTextIOReadChar(var ACh:Char;AUserData:Pointer):Boolean;
function SysTextIOWriteChar(ACh:Char;AUserData:Pointer):Boolean;
function SysTextIOWriteBuffer(ABuffer:PChar;ACount:LongInt;AUserData:Pointer):LongInt;

{==============================================================================}
{Winsock2 Helper Functions}
function Winsock2RedirectInput(s:TSocket):Boolean;
function Winsock2RedirectOutput(s:TSocket):Boolean; 

function Winsock2ErrorToString(AError:LongInt):String;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Winsock2 specific variables}
 WS2Initialized:Boolean;
 
 WS2StartupCount:LongWord;
 WS2StartupError:LongWord;
 WS2StartupLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;

 WS2TlsSize:LongWord;
 WS2TlsIndex:LongWord;
 
 WS2MaxSockets:Word;
 WS2MaxDatagram:Word;

 WS2TextIOInputSocket:TSocket = INVALID_SOCKET;
 WS2TextIOOutputSocket:TSocket = INVALID_SOCKET;
 
{==============================================================================}
{==============================================================================}
{TWinsock2Socket}
constructor TWinsock2Socket.Create;
begin
 {}
 inherited Create;
 FHandle:=INVALID_SOCKET;

 FFamily:=AF_UNSPEC;
 FSocketType:=SOCK_RAW;
 FProtocol:=IPPROTO_IP;

 FLastError:=ERROR_SUCCESS;

 FBoundPort:=0;
 FBoundAddress:='';

 FReuseAddress:=False;
end;

{==============================================================================}

procedure TWinsock2Socket.SetFamily(AFamily:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: SetFamily');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=WSAEINVAL;
 case AFamily of
  AF_UNSPEC,AF_INET,AF_INET6:begin
    {Unspecified, IPv4 or IPv6}
    FLastError:=ERROR_SUCCESS;
    if FFamily <> AFamily then FBoundAddress:='';
    FFamily:=AFamily;
   end;
 end;
end;

{==============================================================================}

procedure TWinsock2Socket.SetSocketType(ASocketType:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: SetSocketType');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 FSocketType:=ASocketType;
end;

{==============================================================================}

procedure TWinsock2Socket.SetProtocol(AProtocol:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: SetProtocol');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 FProtocol:=AProtocol;
end;

{==============================================================================}

procedure TWinsock2Socket.SetBoundPort(ABoundPort:Word);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: SetBoundPort');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  BoundPort = ' + IntToStr(ABoundPort));
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 FBoundPort:=ABoundPort;
end;

{==============================================================================}

procedure TWinsock2Socket.SetBoundAddress(const ABoundAddress:String);
var
 WorkBuffer:String;
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: SetBoundAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  BoundAddress = ' + ABoundAddress);
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 WorkBuffer:=RemoveBraces(ABoundAddress);
 if FBoundAddress = WorkBuffer then Exit;
 FBoundAddress:=WorkBuffer;
 if Length(FBoundAddress) = 0 then
  begin
   FFamily:=AF_UNSPEC;
  end
 else
  begin
   FFamily:=ResolveFamily(FBoundAddress);
  end;
end;

{==============================================================================}

procedure TWinsock2Socket.SetReuseAddress(AReuseAddress:Boolean);
var
 WorkBool:LongBool;
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: SetReuseAddress');
 {$ENDIF}
 
 FLastError:=ERROR_SUCCESS;
 FReuseAddress:=AReuseAddress;

 if not Connected then Exit;

 WorkBool:=FReuseAddress;
 if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_REUSEADDR,PChar(@WorkBool),SizeOf(WorkBool)) = ERROR_SUCCESS then Exit;

 FLastError:=Winsock2.WSAGetLastError;
 
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
 {$ENDIF}
end;

{==============================================================================}

function TWinsock2Socket.GetLocalPort:Word;
var
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: GetLocalPort');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 SockAddrLength:=0;
 SockAddr:=AllocateAddress(SockAddrLength);
 if SockAddr = nil then Exit;
 try
  FLastError:=ERROR_SUCCESS;
  if Winsock2.getsockname(Handle,SockAddr^,SockAddrLength) = SOCKET_ERROR then
   begin
    FLastError:=Winsock2.WSAGetLastError;
    
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  getsockname returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
   end
  else
   begin
    {Check Family}
    case SockAddr.sin_family of
     AF_INET:begin
       {IPv4}
       Result:=Winsock2.ntohs(SockAddr.sin_port);
      end;
     AF_INET6:begin
       {IPv6}
       Result:=Winsock2.ntohs(PSockAddrIn6(SockAddr).sin6_port);
      end;
    end;
    
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Result = ' + IntToStr(Result));
    {$ENDIF}
   end;
 finally
  ReleaseAddress(SockAddr,SockAddrLength);
 end;
end;

{==============================================================================}

function TWinsock2Socket.GetLocalHost:String;
var
 WorkBuffer:String;
begin
 {}
 Result:='';
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: GetLocalHost');
 {$ENDIF}
 
 FLastError:=ERROR_SUCCESS;
 SetLength(WorkBuffer,MAX_PATH);
 if Winsock2.gethostname(PChar(WorkBuffer),Length(WorkBuffer)) = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  gethostname returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end
 else
  begin
   Result:=String(PChar(WorkBuffer));
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Result = ' + Result);
   {$ENDIF}
  end;
end;

{==============================================================================}

function TWinsock2Socket.GetLocalAddress:String;
var
 Addresses:TStrings;
begin
 {}
 Result:='';
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: GetLocalAddress');
 {$ENDIF}
 
 FLastError:=ERROR_SUCCESS;
 Addresses:=GetLocalAddresses;
 if Addresses <> nil then
  begin
   try
    if Addresses.Count > 0 then Result:=Addresses.Strings[0];
    
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Result = ' + Result);
    {$ENDIF}
   finally
    Addresses.Free;
   end;
  end;
end;

{==============================================================================}

function TWinsock2Socket.GetLocalAddresses:TStrings;
{Note: Change this to use GetAddrInfoEx/FreeAddrInfoEx for more control of which addresses to return}
{Note: Change this to use InetNtop for both IPv4 and IPv6 addresses}
var
 Name:String;
 WorkBuffer:String;
 {BufferLength:LongWord;}
 AddrInfo:PAddrInfo;
 NextAddr:PAddrInfo;
 HintsInfo:TAddrInfo;
begin
 {}
 Result:=nil;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: GetLocalAddresses');
 {$ENDIF}
 
 Name:=GetLocalHost;
 AddrInfo:=nil;
 HintsInfo.ai_family:=AF_UNSPEC;
 HintsInfo.ai_protocol:=IPPROTO_IP;
 HintsInfo.ai_socktype:=SOCK_UNSPEC;
 HintsInfo.ai_addrlen:=0;     {Must be zero}
 HintsInfo.ai_canonname:=nil; {Must be nil}
 HintsInfo.ai_addr:=nil;      {Must be nil}
 HintsInfo.ai_next:=nil;      {Must be nil}
 HintsInfo.ai_flags:=AI_ADDRCONFIG;

 FLastError:=ERROR_SUCCESS;
 Result:=TStringList.Create;
 if Winsock2.getaddrinfo(PChar(Name),nil,@HintsInfo,AddrInfo) <> ERROR_SUCCESS then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  getaddrinfo returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end
 else
  begin
   try
    NextAddr:=AddrInfo;
    while NextAddr <> nil do
     begin
      {Check Family}
      case NextAddr.ai_family of
       AF_INET:begin
         {IPv4}
         Result.Add(Winsock2.inet_ntoa(PSockAddrIn(NextAddr.ai_addr).sin_addr));
        end;
       AF_INET6:begin
         {IPv6}
         SetLength(WorkBuffer,INET6_ADDRSTRLEN);
         
         if Winsock2.InetNtopA(AF_INET6,@PSockAddrIn6(NextAddr.ai_addr).sin6_addr,PChar(WorkBuffer),Length(WorkBuffer)) <> nil then
          begin
           Result.Add(String(PChar(WorkBuffer)));
          end; 
         
         {BufferLength:=Length(WorkBuffer);
         if Winsock2.WSAAddressToStringA(PSockAddr(NextAddr.ai_addr)^,NextAddr.ai_addrlen,nil,PChar(WorkBuffer),BufferLength) = ERROR_SUCCESS then
          begin
           Result.Add(String(PChar(WorkBuffer)));
          end;}
        end;
      end;
      NextAddr:=NextAddr.ai_next;
     end;

    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Result Count = ' + IntToStr(Result.Count));
    {$ENDIF}
   finally
    Winsock2.freeaddrinfo(AddrInfo);
   end;
  end;
end;

{==============================================================================}

function TWinsock2Socket.GetSendSize:Integer;
var
 Size:Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: GetSendSize');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 Size:=SizeOf(Integer);
 FLastError:=ERROR_SUCCESS;
 if Winsock2.getsockopt(Handle,SOL_SOCKET,SO_SNDBUF,PChar(@Result),Size) = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  getsockopt returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end;
end;

{==============================================================================}

procedure TWinsock2Socket.SetSendSize(ASize:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: SetSendSize');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_SNDBUF,PChar(@ASize),SizeOf(Integer)) = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end;
end;

{==============================================================================}

function TWinsock2Socket.GetReceiveSize:Integer;
var
 Size:Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: GetReceiveSize');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 Size:=SizeOf(Integer);
 FLastError:=ERROR_SUCCESS;
 if Winsock2.getsockopt(Handle,SOL_SOCKET,SO_RCVBUF,PChar(@Result),Size) = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  getsockopt returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end;
end;

{==============================================================================}

procedure TWinsock2Socket.SetReceiveSize(ASize:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: SetReceiveSize');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_RCVBUF,PChar(@ASize),SizeOf(Integer)) = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end;
end;

{==============================================================================}

function TWinsock2Socket.GetSendTimeout:Integer;
var
 Size:Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: GetSendTimeout');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 Size:=SizeOf(Integer);
 FLastError:=ERROR_SUCCESS;
 if Winsock2.getsockopt(Handle,SOL_SOCKET,SO_SNDTIMEO,PChar(@Result),Size) = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  getsockopt returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end;
end;

{==============================================================================}

procedure TWinsock2Socket.SetSendTimeout(ATimeout:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: SetSendTimeout');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_SNDTIMEO,PChar(@ATimeout),SizeOf(Integer)) = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end;
end;

{==============================================================================}

function TWinsock2Socket.GetReceiveTimeout:Integer;
var
 Size:Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: GetReceiveTimeout');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 Size:=SizeOf(Integer);
 FLastError:=ERROR_SUCCESS;
 if Winsock2.getsockopt(Handle,SOL_SOCKET,SO_RCVTIMEO,PChar(@Result),Size) = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  getsockopt returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end;
end;

{==============================================================================}

procedure TWinsock2Socket.SetReceiveTimeout(ATimeout:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: SetReceiveTimeout');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_RCVTIMEO,PChar(@ATimeout),SizeOf(Integer)) = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end;
end;

{==============================================================================}

function TWinsock2Socket.GetConnectTimeout:Integer;
var
 Size:Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: GetConnectTimeout');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 Size:=SizeOf(Integer);
 FLastError:=ERROR_SUCCESS;
 if Winsock2.getsockopt(Handle,SOL_SOCKET,SO_CONNTIMEO,PChar(@Result),Size) = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  getsockopt returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end;
end;

{==============================================================================}

procedure TWinsock2Socket.SetConnectTimeout(ATimeout:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: SetConnectTimeout');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_CONNTIMEO,PChar(@ATimeout),SizeOf(Integer)) = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end;
end;

{==============================================================================}

function TWinsock2Socket.GetBroadcastAddress:String;
begin
 {}
 Result:='';
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;
 
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: GetBroadcastAddress');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;
 
 {Check Family}
 FLastError:=WSAEINVAL;
 case FFamily of
  AF_INET:begin
    {IPv4}
    FLastError:=ERROR_SUCCESS;
    Result:=INET_ADDRSTR_BROADCAST;
   end;
  AF_INET6:begin
    {IPv6}
    FLastError:=WSAEINVAL;
    Result:=''; {No broadcast in IPv6}
   end;
 end;
 
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Result = ' + Result);
 {$ENDIF}
end;

{==============================================================================}

function TWinsock2Socket.AllocateFamily:Boolean;
{Check Family and Bound Address, normalise as needed}
var
 WorkInt:Integer;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: AllocateFamily');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=WSAEINVAL;
 {Check Bound Address}
 if Length(FBoundAddress) = 0 then
  begin
   {Check Family}
   case FFamily of
    AF_UNSPEC:begin
      {Unspecified}
      FFamily:=AF_INET;
      FBoundAddress:=INET_ADDRSTR_ANY;
      FLastError:=ERROR_SUCCESS;
      Result:=True;
     end;
    AF_INET:begin
      {IPv4}
      FBoundAddress:=INET_ADDRSTR_ANY;
      FLastError:=ERROR_SUCCESS;
      Result:=True;
     end;
    AF_INET6:begin
      {IPv6}
      FBoundAddress:=INET6_ADDRSTR_INIT;
      FLastError:=ERROR_SUCCESS;
      Result:=True;
     end;
   end;
  end
 else
  begin
   {Check Family}
   case FFamily of
    AF_UNSPEC:begin
      {Unspecified}
      FFamily:=ResolveFamily(FBoundAddress);
      FLastError:=ERROR_SUCCESS;
      Result:=True;
     end;
    AF_INET:begin
      {IPv4}
      WorkInt:=ResolveFamily(FBoundAddress);
      if WorkInt <> FFamily then
       begin
        FBoundAddress:=INET_ADDRSTR_ANY;
       end;
      FLastError:=ERROR_SUCCESS;
      Result:=True;
     end;
    AF_INET6:begin
      {IPv6}
      WorkInt:=ResolveFamily(FBoundAddress);
      if WorkInt <> FFamily then
       begin
        FBoundAddress:=INET6_ADDRSTR_INIT;
       end;
      FLastError:=ERROR_SUCCESS;
      Result:=True;
     end;
   end;
  end;
  
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  BoundAddress = ' + FBoundAddress);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Family = ' + AddressFamilyToString(FFamily));
 {$ENDIF}
end;

{==============================================================================}

function TWinsock2Socket.AllocateAddress(var ALength:Integer):PSockAddr;
{Allocate s socket address structure for the current family}
begin
 {}
 Result:=nil;
 ALength:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: AllocateAddress');
 {$ENDIF}
 
 {Check Family}
 FLastError:=WSAEINVAL;
 case FFamily of
  AF_INET:begin
    {IPv4}
    Result:=AllocMem(SizeOf(TSockAddrIn));
    if Result = nil then Exit;
    ALength:=SizeOf(TSockAddrIn);
    {Setup address}
    Result.sin_family:=FFamily;
    FLastError:=ERROR_SUCCESS;
   end;
  AF_INET6:begin
    {IPv6}
    Result:=AllocMem(SizeOf(TSockAddrIn6));
    if Result = nil then Exit;
    ALength:=SizeOf(TSockAddrIn6);
    {Setup address}
    PSockAddrIn6(Result).sin6_family:=FFamily;
    FLastError:=ERROR_SUCCESS;
   end;
 end;
end;

{==============================================================================}

function TWinsock2Socket.ReleaseAddress(var ASockAddr:PSockAddr;var ALength:Integer):Boolean;
{Free a socket address structure allocated by AllocateAddress or AddressToSockAddr}
begin
 {}
 Result:=False;
 try
  FLastError:=WSAEINVAL;
  if ASockAddr = nil then Exit;

  {$IFDEF WINSOCK2_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: ReleaseAddress');
  {$ENDIF}
  
  FreeMem(ASockAddr);
  ASockAddr:=nil;
  ALength:=0;
  FLastError:=ERROR_SUCCESS;
  Result:=True;
 except
  {}
 end;
end;

{==============================================================================}

function TWinsock2Socket.AllocateBoundAddress(var ALength:Integer):PSockAddr;
{Allocate a socket address structure for the bound address and port}
{Note: Change this to use InetPton for both IPv4 and IPv6 addresses}
var
 InAddr:TInAddr;
 SockAddr:TSockAddrIn6;
 {BufferLength:LongInt;}
begin
 {}
 Result:=nil;
 ALength:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: AllocateBoundAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  BoundAddress = ' + FBoundAddress);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  BoundPort = ' + IntToStr(FBoundPort));
 {$ENDIF}
 
 {Allocate Address}
 Result:=AllocateAddress(ALength);
 if Result = nil then Exit;

 {Check Family}
 case Result.sin_family of
  AF_INET:begin
    {IPv4}
    {Setup bound address}
    Result.sin_port:=Winsock2.htons(FBoundPort);
    Result.sin_addr.S_addr:=INADDR_ANY;
    if (Length(FBoundAddress) <> 0) and (FBoundAddress <> INET_ADDRSTR_ANY)then
     begin
      FLastError:=WSAEINVAL;
      InAddr.S_addr:=Winsock2.inet_addr(PChar(FBoundAddress));
      if InAddr.S_addr = INADDR_NONE then
       begin
        FLastError:=WSAEINVAL;
        ReleaseAddress(Result,ALength);
        
        {$IFDEF WINSOCK2_DEBUG}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  inet_addr returned: = INADDR_NONE');
        {$ENDIF}
        
        Exit;
       end; 
      if (InAddr.S_addr = INADDR_ANY) and (FBoundAddress <> INET_ADDRSTR_ANY) then
       begin
        FLastError:=WSAEINVAL;
        ReleaseAddress(Result,ALength);

        {$IFDEF WINSOCK2_DEBUG}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  inet_addr returned: = INADDR_ANY');
        {$ENDIF}
        
        Exit;
       end; 
      Result.sin_addr.S_addr:=InAddr.S_addr;
     end;
    FLastError:=ERROR_SUCCESS;
   end;
  AF_INET6:begin
    {IPv6}
    {Setup bound address}
    PSockAddrIn6(Result).sin6_port:=Winsock2.htons(FBoundPort);
    PSockAddrIn6(Result).sin6_addr.s6_addr:=IN6ADDR_ANY_INIT.s6_addr;
    if (Length(FBoundAddress) <> 0) and (FBoundAddress <> INET6_ADDRSTR_INIT) then
     begin
      if Winsock2.InetPtonA(AF_INET6,PChar(FBoundAddress),@SockAddr.sin6_addr) = SOCKET_ERROR then
       begin
        FLastError:=Winsock2.WSAGetLastError;
        ReleaseAddress(Result,ALength);

        {$IFDEF WINSOCK2_DEBUG}
        if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  InetPtonA returned: ' + Winsock2ErrorToString(FLastError));
        {$ENDIF}
        
        Exit;
       end;
      
      {BufferLength:=SizeOf(TSockAddrIn6);
      if Winsock2.WSAStringToAddressA(PChar(FBoundAddress),FFamily,nil,PSockAddrIn(@SockAddr)^,BufferLength) = SOCKET_ERROR then
       begin
        FLastError:=Winsock2.WSAGetLastError;
        ReleaseAddress(Result,ALength);
        Exit;
       end;}
       
      PSockAddrIn6(Result).sin6_addr.s6_addr:=SockAddr.sin6_addr.s6_addr;
     end;
    FLastError:=ERROR_SUCCESS;
   end;
 end;
end;

{==============================================================================}

function TWinsock2Socket.AllocateSocket(ASocketType:Integer):TSocket;
begin
 {}
 Result:=INVALID_SOCKET;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: AllocateSocket');
 {$ENDIF}
 
 FSocketType:=ASocketType;
 FLastError:=ERROR_SUCCESS;
 FHandle:=Winsock2.socket(FFamily,FSocketType,FProtocol);
 if FHandle = INVALID_SOCKET then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  socket returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end
 else
  begin
   Result:=FHandle;
  end;
end;

{==============================================================================}

function TWinsock2Socket.Bind:LongInt;
var
 WorkBool:LongBool;
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
begin
 {}
 Result:=SOCKET_ERROR;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: Bind');
 {$ENDIF}

 {Set Options}
 if ReuseAddress then
  begin
   WorkBool:=ReuseAddress;
   if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_REUSEADDR,PChar(@WorkBool),SizeOf(WorkBool)) = SOCKET_ERROR then
    begin
     FLastError:=Winsock2.WSAGetLastError;
      
     {$IFDEF WINSOCK2_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
     {$ENDIF}
      
     Exit;
    end;
  end;
 
 SockAddrLength:=0;
 SockAddr:=AllocateBoundAddress(SockAddrLength);
 if SockAddr = nil then Exit;
 try
  FLastError:=ERROR_SUCCESS;
  Result:=Winsock2.bind(Handle,SockAddr^,SockAddrLength);
  if Result = SOCKET_ERROR then
   begin
    FLastError:=Winsock2.WSAGetLastError;
    
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  bind returned: = ' + Winsock2ErrorToString(FLastError));
    {$ENDIF}
   end;
 finally
  ReleaseAddress(SockAddr,SockAddrLength);
 end;
end;

{==============================================================================}

function TWinsock2Socket.SockAddrToPort(ASockAddr:PSockAddr;ALength:Integer):Word;
{Convert a socket adress structure to a numeric port}
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: SockAddrToPort');
 {$ENDIF}
 
 FLastError:=WSAEINVAL;
 if ASockAddr = nil then Exit;

 {Check Family}
 FLastError:=ERROR_SUCCESS;
 case ASockAddr.sin_family of
  AF_INET:begin
    {IPv4}
    Result:=Winsock2.ntohs(ASockAddr.sin_port);
   end;
  AF_INET6:begin
    {IPv6}
    Result:=Winsock2.ntohs(PSockAddrIn6(ASockAddr).sin6_port);
   end;
 end;
end;

{==============================================================================}

function TWinsock2Socket.SockAddrToAddress(ASockAddr:PSockAddr;ALength:Integer):String;
{Convert a socket address structure to a numeric address}
var
 WorkBuffer:String;
begin
 {}
 Result:='';
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: SockAddrToAddress');
 {$ENDIF}
 
 FLastError:=WSAEINVAL;
 if ASockAddr = nil then Exit;

 FLastError:=ERROR_SUCCESS;
 SetLength(WorkBuffer,NI_MAXHOST);
 if Winsock2.getnameinfo(ASockAddr,ALength,PChar(WorkBuffer),NI_MAXHOST,nil,0,NI_NUMERICHOST) <> ERROR_SUCCESS then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  getnameinfo returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end
 else
  begin
   Result:=String(PChar(WorkBuffer));
  end;
end;

{==============================================================================}

function TWinsock2Socket.PortToSockAddr(APort:Word;ASockAddr:PSockAddr;ALength:Integer):Boolean;
{Convert a numeric port to a socket adress structure}
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: PortToSockAddr');
 {$ENDIF}
 
 FLastError:=WSAEINVAL;
 if ASockAddr = nil then Exit;

 {Check Family}
 FLastError:=ERROR_SUCCESS;
 Result:=True;
 case ASockAddr.sin_family of
  AF_INET:begin
    {IPv4}
    ASockAddr.sin_port:=Winsock2.htons(APort);
   end;
  AF_INET6:begin
    {IPv6}
    PSockAddrIn6(ASockAddr).sin6_port:=Winsock2.htons(APort);
   end;
 end;
end;
 
{==============================================================================}

function TWinsock2Socket.AddressToSockAddr(const AAddress:String;var ALength:Integer):PSockAddr;
{Convert a numeric address to a socket address structure}
var
 AddrInfo:PAddrInfo;
 HintsInfo:TAddrInfo;
begin
 {}
 Result:=nil;
 ALength:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: AddressToSockAddr');
 {$ENDIF}
 
 AddrInfo:=nil;
 HintsInfo.ai_family:=AF_UNSPEC;
 HintsInfo.ai_protocol:=IPPROTO_IP;
 HintsInfo.ai_socktype:=SOCK_UNSPEC;
 HintsInfo.ai_addrlen:=0;     {Must be zero}
 HintsInfo.ai_canonname:=nil; {Must be nil}
 HintsInfo.ai_addr:=nil;      {Must be nil}
 HintsInfo.ai_next:=nil;      {Must be nil}
 HintsInfo.ai_flags:=AI_NUMERICHOST;

 FLastError:=WSAEINVAL;
 if Winsock2.getaddrinfo(PChar(AAddress),nil,@HintsInfo,AddrInfo) <> ERROR_SUCCESS then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  getaddrinfo returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end
 else
  begin
   try
    if AddrInfo <> nil then
     begin
      {Check Family}
      case AddrInfo.ai_family of
       AF_INET:begin
         {IPv4}
         Result:=AllocMem(SizeOf(TSockAddrIn));
         if Result = nil then Exit;
         ALength:=SizeOf(TSockAddrIn);
         Result.sin_family:=AddrInfo.ai_addr.sin_family;
         Result.sin_port:=AddrInfo.ai_addr.sin_port;
         Result.sin_addr:=AddrInfo.ai_addr.sin_addr;
         Result.sin_zero:=AddrInfo.ai_addr.sin_zero;
         FLastError:=ERROR_SUCCESS;
        end;
       AF_INET6:begin
         {IPv6}
         Result:=AllocMem(SizeOf(TSockAddrIn6));
         if Result = nil then Exit;
         ALength:=SizeOf(TSockAddrIn6);
         PSockAddrIn6(Result).sin6_family:=PSockAddrIn6(AddrInfo.ai_addr).sin6_family;
         PSockAddrIn6(Result).sin6_port:=PSockAddrIn6(AddrInfo.ai_addr).sin6_port;
         PSockAddrIn6(Result).sin6_flowinfo:=PSockAddrIn6(AddrInfo.ai_addr).sin6_flowinfo;
         PSockAddrIn6(Result).sin6_addr:=PSockAddrIn6(AddrInfo.ai_addr).sin6_addr;
         PSockAddrIn6(Result).sin6_scope_id:=PSockAddrIn6(AddrInfo.ai_addr).sin6_scope_id;
         FLastError:=ERROR_SUCCESS;
        end;
      end;
     end;
   finally
    Winsock2.freeaddrinfo(AddrInfo);
   end;
  end;
end;

{==============================================================================}

function TWinsock2Socket.Connected:Boolean;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=ERROR_SUCCESS;
 Result:=FHandle <> INVALID_SOCKET;
end;

{==============================================================================}

function TWinsock2Socket.Shutdown:Boolean;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: Shutdown');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 if Winsock2.shutdown(FHandle,SD_BOTH) = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  shutdown returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end
 else
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function TWinsock2Socket.Disconnect:Boolean;
begin
 {}
 if Connected then
  begin
   Result:=CloseSocket;
  end
 else
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function TWinsock2Socket.CloseSocket:Boolean;
var
 OldHandle:THandle;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: CloseSocket');
 {$ENDIF}
 
 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 {Save the handle, other threads may check FHandle when socket closes}
 OldHandle:=FHandle;
 FHandle:=INVALID_SOCKET;

 FLastError:=ERROR_SUCCESS;
 if Winsock2.closesocket(OldHandle) = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  closesocket returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end
 else
  begin
   Result:=True;
  end;
end;

{==============================================================================}

function TWinsock2Socket.ResolveHost(const AHost:String):String;
{Resolve a hostname to a numeric address}
var
 WorkBuffer:String;
 {BufferLength:LongWord;}
 AddrInfo:PAddrInfo;
 NextAddr:PAddrInfo;
 HintsInfo:TAddrInfo;
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
begin
 {}
 Result:='';
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: ResolveHost');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Host = ' + AHost);
 {$ENDIF}
 
 {Check Numeric}
 SockAddrLength:=0;
 SockAddr:=AddressToSockAddr(AHost,SockAddrLength);
 if SockAddr <> nil then
  begin
   try
    FLastError:=ERROR_SUCCESS;
    
    case SockAddr.sin_family of
     AF_INET:begin
       {IPv4}
       Result:=Winsock2.inet_ntoa(SockAddr.sin_addr);
       
       {$IFDEF WINSOCK2_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Result = ' + Result);
       {$ENDIF}
      end;
     AF_INET6:begin
       {IPv6}
       SetLength(WorkBuffer,INET6_ADDRSTRLEN);
       
       if Winsock2.InetNtopA(AF_INET6,@PSockAddrIn6(SockAddr).sin6_addr,PChar(WorkBuffer),Length(WorkBuffer)) <> nil then
        begin
         Result:=String(PChar(WorkBuffer));
         
         {$IFDEF WINSOCK2_DEBUG}
         if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Result = ' + Result);
         {$ENDIF}
        end; 
      end;
    end;
   finally
    ReleaseAddress(SockAddr,SockAddrLength);
   end;
  end
 else
  begin
   AddrInfo:=nil;
   HintsInfo.ai_family:=FFamily;
   HintsInfo.ai_protocol:=IPPROTO_IP;
   HintsInfo.ai_socktype:=SOCK_UNSPEC;
   HintsInfo.ai_addrlen:=0;     {Must be zero}
   HintsInfo.ai_canonname:=nil; {Must be nil}
   HintsInfo.ai_addr:=nil;      {Must be nil}
   HintsInfo.ai_next:=nil;      {Must be nil}
   HintsInfo.ai_flags:=AI_ADDRCONFIG;
  
   FLastError:=ERROR_SUCCESS;
   if Winsock2.getaddrinfo(PChar(AHost),nil,@HintsInfo,AddrInfo) <> ERROR_SUCCESS then
    begin
     FLastError:=Winsock2.WSAGetLastError;
     
     {$IFDEF WINSOCK2_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  getaddrinfo returned: = ' + Winsock2ErrorToString(FLastError));
     {$ENDIF}
    end
   else
    begin
     try
      NextAddr:=AddrInfo;
      while NextAddr <> nil do
       begin
        case NextAddr.ai_family of
         AF_INET:begin
           {IPv4}
           Result:=Winsock2.inet_ntoa(PSockAddrIn(NextAddr.ai_addr).sin_addr);
           
           {$IFDEF WINSOCK2_DEBUG}
           if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Result = ' + Result);
           {$ENDIF}
           
           Exit;
          end;
         AF_INET6:begin
           {IPv6}
           SetLength(WorkBuffer,INET6_ADDRSTRLEN);
           
           if Winsock2.InetNtopA(AF_INET6,@PSockAddrIn6(NextAddr.ai_addr).sin6_addr,PChar(WorkBuffer),Length(WorkBuffer)) <> nil then
            begin
             Result:=String(PChar(WorkBuffer));
             
             {$IFDEF WINSOCK2_DEBUG}
             if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Result = ' + Result);
             {$ENDIF}
             
             Exit;
            end; 
           
           {BufferLength:=Length(WorkBuffer);
           if Winsock2.WSAAddressToStringA(PSockAddr(NextAddr.ai_addr)^,NextAddr.ai_addrlen,nil,PChar(WorkBuffer),BufferLength) = ERROR_SUCCESS then
            begin
             Result:=String(PChar(WorkBuffer));
             Exit;
            end;}
          end;
        end;
        NextAddr:=NextAddr.ai_next;
       end;
     finally
      Winsock2.freeaddrinfo(AddrInfo);
     end;
    end;
  end;
end;

{==============================================================================}

function TWinsock2Socket.ResolveAddress(const AAddress:String):String;
{Resolve a numeric address to a hostname}
var
 WorkBuffer:String;
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
begin
 {}
 Result:='';
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: ResolveAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Address = ' + AAddress);
 {$ENDIF}
 
 SockAddrLength:=0;
 SockAddr:=AddressToSockAddr(AAddress,SockAddrLength);
 if SockAddr = nil then Exit;
 try
  FLastError:=ERROR_SUCCESS;
  SetLength(WorkBuffer,NI_MAXHOST);
  if Winsock2.getnameinfo(SockAddr,SockAddrLength,PChar(WorkBuffer),NI_MAXHOST,nil,0,NI_NAMEREQD) <> ERROR_SUCCESS then
   begin
    FLastError:=Winsock2.WSAGetLastError;
    
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  getnameinfo returned: = ' + Winsock2ErrorToString(FLastError));
    {$ENDIF}
   end
  else
   begin
    Result:=String(PChar(WorkBuffer));
    
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Result = ' + Result);
    {$ENDIF}
   end;
 finally
  ReleaseAddress(SockAddr,SockAddrLength);
 end; 
end;

{==============================================================================}

function TWinsock2Socket.ResolveFamily(const AAddress:String):Integer;
{Resolve a numeric address to an address family}
var
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
begin
 {}
 Result:=AF_UNSPEC;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: ResolveFamily');
 {$ENDIF}
 
 SockAddrLength:=0;
 SockAddr:=AddressToSockAddr(AAddress,SockAddrLength);
 if SockAddr = nil then Exit;
 try
  FLastError:=ERROR_SUCCESS;
  Result:=SockAddr.sin_family;
  
  {$IFDEF WINSOCK2_DEBUG}
  if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Result = ' + AddressFamilyToString(Result));
  {$ENDIF}
 finally
  ReleaseAddress(SockAddr,SockAddrLength);
 end; 
end;

{==============================================================================}

function TWinsock2Socket.ResolveHostEx(const AHost:String;AFamily:Integer;AAll:Boolean):TStrings;
{Resolve a hostname to a numeric address or addresses in the family specified or all families}
{Note All may include addresses that this host cannot connect to such as IPv6 if this host does not have a global IPv6 address}
var
 WorkBuffer:String;
 {BufferLength:LongWord;}
 AddrInfo:PAddrInfo;
 NextAddr:PAddrInfo;
 HintsInfo:TAddrInfo;
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
begin
 {}
 Result:=nil;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket: ResolveHostEx');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Host = ' + AHost);
 {$ENDIF}
 
 {Check Numeric}
 SockAddrLength:=0;
 SockAddr:=AddressToSockAddr(AHost,SockAddrLength);
 if SockAddr <> nil then
  begin
   try
    FLastError:=ERROR_SUCCESS;
    
    case SockAddr.sin_family of
     AF_INET:begin
       {IPv4}
       Result.Add(Winsock2.inet_ntoa(SockAddr.sin_addr));
      end;
     AF_INET6:begin
       {IPv6}
       SetLength(WorkBuffer,INET6_ADDRSTRLEN);
       
       if Winsock2.InetNtopA(AF_INET6,@PSockAddrIn6(SockAddr).sin6_addr,PChar(WorkBuffer),Length(WorkBuffer)) <> nil then
        begin
         Result.Add(String(PChar(WorkBuffer)));
        end; 
      end;
    end;
   finally
    ReleaseAddress(SockAddr,SockAddrLength);
   end;
  end
 else
  begin 
   AddrInfo:=nil;
   HintsInfo.ai_family:=AFamily;
   HintsInfo.ai_protocol:=IPPROTO_IP;
   HintsInfo.ai_socktype:=SOCK_UNSPEC;
   HintsInfo.ai_addrlen:=0;     {Must be zero}
   HintsInfo.ai_canonname:=nil; {Must be nil}
   HintsInfo.ai_addr:=nil;      {Must be nil}
   HintsInfo.ai_next:=nil;      {Must be nil}
   if AAll then HintsInfo.ai_flags:=AI_ALL else HintsInfo.ai_flags:=AI_ADDRCONFIG;
  
   FLastError:=ERROR_SUCCESS;
   Result:=TStringList.Create;
   if Winsock2.getaddrinfo(PChar(AHost),nil,@HintsInfo,AddrInfo) <> ERROR_SUCCESS then
    begin
     FLastError:=Winsock2.WSAGetLastError;
     
     {$IFDEF WINSOCK2_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  getaddrinfo returned: = ' + Winsock2ErrorToString(FLastError));
     {$ENDIF}
    end
   else
    begin
     try
      NextAddr:=AddrInfo;
      while NextAddr <> nil do
       begin
        case NextAddr.ai_family of
         AF_INET:begin
           {IPv4}
           Result.Add(Winsock2.inet_ntoa(PSockAddrIn(NextAddr.ai_addr).sin_addr));
          end;
         AF_INET6:begin
           {IPv6}
           SetLength(WorkBuffer,INET6_ADDRSTRLEN);
           
           if Winsock2.InetNtopA(AF_INET6,@PSockAddrIn6(NextAddr.ai_addr).sin6_addr,PChar(WorkBuffer),Length(WorkBuffer)) <> nil then
            begin
             Result.Add(String(PChar(WorkBuffer)));
            end; 
           
           {BufferLength:=Length(WorkBuffer);
           if Winsock2.WSAAddressToStringA(PSockAddr(NextAddr.ai_addr)^,NextAddr.ai_addrlen,nil,PChar(WorkBuffer),BufferLength) = ERROR_SUCCESS then
            begin
             Result.Add(String(PChar(WorkBuffer)));
            end;}
          end;
        end;
        NextAddr:=NextAddr.ai_next;
       end;
       
      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 Socket:  Result Count = ' + IntToStr(Result.Count));
      {$ENDIF}
     finally
      Winsock2.freeaddrinfo(AddrInfo);
     end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{TWinsock2SocketThread}

{==============================================================================}
{==============================================================================}
{TWinsock2SocketThreads}
constructor TWinsock2SocketThreads.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;

 FCount:=0;
 FFirst:=nil;
 FLast:=nil;
end;

{==============================================================================}

destructor TWinsock2SocketThreads.Destroy;
begin
 {}
 AcquireLock;
 try
  Clear;
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TWinsock2SocketThreads.GetCount:Integer;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FCount;

 ReleaseLock;
end;

{==============================================================================}

function TWinsock2SocketThreads.GetFirst:TWinsock2SocketThread;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;

 Result:=FFirst;

 ReleaseLock;
end;

{==============================================================================}

function TWinsock2SocketThreads.GetLast:TWinsock2SocketThread;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;

 Result:=FLast;

 ReleaseLock;
end;

{==============================================================================}

function TWinsock2SocketThreads.Link(AValue:TWinsock2SocketThread):Boolean;
{Link AValue to Prev,Next Siblings and Adjust First/Last}
var
 Prev:TWinsock2SocketThread;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 Prev:=FLast;
 if Prev = nil then
  begin
   {Is First Object}
   AValue.Prev:=nil;
   AValue.Next:=nil;
   FFirst:=AValue;
   FLast:=AValue;
  end
 else
  begin
   {Not First Object}
   Prev.Next:=AValue;
   AValue.Prev:=Prev;
   AValue.Next:=nil;
   FLast:=AValue;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TWinsock2SocketThreads.LinkEx(APrev,AValue:TWinsock2SocketThread):Boolean;
{Link AValue after APrev Sibling and Adjust First/Last/Prev/Next}
{If APrev is nil then Link as first value in list}
var
 Next:TWinsock2SocketThread;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if APrev = nil then
  begin
   if FLast <> nil then
    begin
     {Not First Object}
     Next:=FFirst;
     FFirst:=AValue;
     AValue.Prev:=nil;
     AValue.Next:=Next;
     Next.Prev:=AValue;
    end
   else
    begin
     {Is First Object}
     AValue.Prev:=nil;
     AValue.Next:=nil;
     FFirst:=AValue;
     FLast:=AValue;
    end;
  end
 else
  begin
   if APrev.Next <> nil then
    begin
     {Not Last Object}
     Next:=APrev.Next;
     APrev.Next:=AValue;
     AValue.Prev:=APrev;
     AValue.Next:=Next;
     Next.Prev:=AValue;
    end
   else
    begin
     {Is Last Object}
     APrev.Next:=AValue;
     AValue.Prev:=APrev;
     AValue.Next:=nil;
     FLast:=AValue;
    end;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TWinsock2SocketThreads.Unlink(AValue:TWinsock2SocketThread):Boolean;
{Unlink AValue from Prev,Next Siblings and Adjust First/Last}
var
 Prev:TWinsock2SocketThread;
 Next:TWinsock2SocketThread;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if AValue.Prev <> nil then
  begin
   {Not First Object}
   Prev:=AValue.Prev;
   if AValue.Next <> nil then
    begin
     {Not Last Object}
     Next:=AValue.Next;
     Prev.Next:=Next;
     Next.Prev:=Prev;
    end
   else
    begin
     {Is Last Object}
     Prev.Next:=nil;
     FLast:=Prev;
    end;
  end
 else
  begin
   {Is First Object}
   if AValue.Next <> nil then
    begin
     {Not Last Object}
     Next:=AValue.Next;
     Next.Prev:=nil;
     FFirst:=Next;
    end
   else
    begin
     {Is Last Object}
     FFirst:=nil;
     FLast:=nil;
    end;
  end;
 AValue.Prev:=nil;
 AValue.Next:=nil;
 
 Result:=True;
end;

{==============================================================================}

function TWinsock2SocketThreads.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TWinsock2SocketThreads.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TWinsock2SocketThreads.Add(AValue:TWinsock2SocketThread):Boolean;
{Add AValue to List and link with Siblings}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  if AValue = nil then Exit;
  
  if Find(AValue) then Exit;
  
  if Link(AValue) then
   begin
    Inc(FCount);
    Result:=True;
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TWinsock2SocketThreads.Remove(AValue:TWinsock2SocketThread):Boolean;
{Unlink AValue from Siblings and Remove from List}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  if AValue = nil then Exit;
  
  if not Find(AValue) then Exit;
  
  if Unlink(AValue) then
   begin
    Dec(FCount);
    Result:=True;
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TWinsock2SocketThreads.Insert(APrev,AValue:TWinsock2SocketThread):Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  if AValue = nil then Exit;
  
  if Find(AValue) then Exit;
  
  if LinkEx(APrev,AValue) then
   begin
    Inc(FCount);
    Result:=True;
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TWinsock2SocketThreads.Find(AValue:TWinsock2SocketThread):Boolean;
var
 Next:TWinsock2SocketThread;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  if AValue = nil then Exit;
  
  Next:=FFirst;
  while Next <> nil do
   begin
    if Next = AValue then
     begin
      Result:=True;
      Exit;
     end;
     
    Next:=Next.Next;
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TWinsock2SocketThreads.FindByID(AThreadID:TThreadID):TWinsock2SocketThread; 
var
 Next:TWinsock2SocketThread;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  Next:=FFirst;
  while Next <> nil do
   begin
    if Next.ThreadID = AThreadID then
     begin
      Result:=Next;
      Exit;
     end;
     
    Next:=Next.Next;
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

procedure TWinsock2SocketThreads.Clear;
begin
 {}
 if not AcquireLock then Exit;

 FCount:=0;
 FFirst:=nil;
 FLast:=nil;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TWinsock2SocketBuffer}

{==============================================================================}
{==============================================================================}
{TWinsock2SocketBuffers}
constructor TWinsock2SocketBuffers.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;

 FCount:=0;
 FFirst:=nil;
 FLast:=nil;
end;

{==============================================================================}

destructor TWinsock2SocketBuffers.Destroy; 
begin
 {}
 AcquireLock;
 try
  Clear;
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TWinsock2SocketBuffers.GetCount:Integer;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FCount;

 ReleaseLock;
end;

{==============================================================================}

function TWinsock2SocketBuffers.GetFirst:TWinsock2SocketBuffer;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;

 Result:=FFirst;

 ReleaseLock;
end;

{==============================================================================}

function TWinsock2SocketBuffers.GetLast:TWinsock2SocketBuffer;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;

 Result:=FLast;

 ReleaseLock;
end;

{==============================================================================}

function TWinsock2SocketBuffers.Link(AValue:TWinsock2SocketBuffer):Boolean;
{Link AValue to Prev,Next Siblings and Adjust First/Last}
var
 Prev:TWinsock2SocketBuffer;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 Prev:=FLast;
 if Prev = nil then
  begin
   {Is First Object}
   AValue.Prev:=nil;
   AValue.Next:=nil;
   FFirst:=AValue;
   FLast:=AValue;
  end
 else
  begin
   {Not First Object}
   Prev.Next:=AValue;
   AValue.Prev:=Prev;
   AValue.Next:=nil;
   FLast:=AValue;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TWinsock2SocketBuffers.LinkEx(APrev,AValue:TWinsock2SocketBuffer):Boolean;
{Link AValue after APrev Sibling and Adjust First/Last/Prev/Next}
{If APrev is nil then Link as first value in list}
var
 Next:TWinsock2SocketBuffer;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if APrev = nil then
  begin
   if FLast <> nil then
    begin
     {Not First Object}
     Next:=FFirst;
     FFirst:=AValue;
     AValue.Prev:=nil;
     AValue.Next:=Next;
     Next.Prev:=AValue;
    end
   else
    begin
     {Is First Object}
     AValue.Prev:=nil;
     AValue.Next:=nil;
     FFirst:=AValue;
     FLast:=AValue;
    end;
  end
 else
  begin
   if APrev.Next <> nil then
    begin
     {Not Last Object}
     Next:=APrev.Next;
     APrev.Next:=AValue;
     AValue.Prev:=APrev;
     AValue.Next:=Next;
     Next.Prev:=AValue;
    end
   else
    begin
     {Is Last Object}
     APrev.Next:=AValue;
     AValue.Prev:=APrev;
     AValue.Next:=nil;
     FLast:=AValue;
    end;
  end;
  
 Result:=True;
end;

{==============================================================================}

function TWinsock2SocketBuffers.Unlink(AValue:TWinsock2SocketBuffer):Boolean;
{Unlink AValue from Prev,Next Siblings and Adjust First/Last}
var
 Prev:TWinsock2SocketBuffer;
 Next:TWinsock2SocketBuffer;
begin
 {}
 Result:=False;
 
 if AValue = nil then Exit;
 
 if AValue.Prev <> nil then
  begin
   {Not First Object}
   Prev:=AValue.Prev;
   if AValue.Next <> nil then
    begin
     {Not Last Object}
     Next:=AValue.Next;
     Prev.Next:=Next;
     Next.Prev:=Prev;
    end
   else
    begin
     {Is Last Object}
     Prev.Next:=nil;
     FLast:=Prev;
    end;
  end
 else
  begin
   {Is First Object}
   if AValue.Next <> nil then
    begin
     {Not Last Object}
     Next:=AValue.Next;
     Next.Prev:=nil;
     FFirst:=Next;
    end
   else
    begin
     {Is Last Object}
     FFirst:=nil;
     FLast:=nil;
    end;
  end;
 AValue.Prev:=nil;
 AValue.Next:=nil;
 
 Result:=True;
end;

{==============================================================================}

function TWinsock2SocketBuffers.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TWinsock2SocketBuffers.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TWinsock2SocketBuffers.Add(AValue:TWinsock2SocketBuffer):Boolean; 
{Add AValue to List and link with Siblings}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  if AValue = nil then Exit;
  
  if Find(AValue) then Exit;
  
  if Link(AValue) then
   begin
    Inc(FCount);
    Result:=True;
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TWinsock2SocketBuffers.Remove(AValue:TWinsock2SocketBuffer):Boolean; 
{Unlink AValue from Siblings and Remove from List}
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  if AValue = nil then Exit;
  
  if not Find(AValue) then Exit;
  
  if Unlink(AValue) then
   begin
    Dec(FCount);
    Result:=True;
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TWinsock2SocketBuffers.Insert(APrev,AValue:TWinsock2SocketBuffer):Boolean; 
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  if AValue = nil then Exit;
  
  if Find(AValue) then Exit;
  
  if LinkEx(APrev,AValue) then
   begin
    Inc(FCount);
    Result:=True;
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

function TWinsock2SocketBuffers.Find(AValue:TWinsock2SocketBuffer):Boolean; 
var
 Next:TWinsock2SocketBuffer;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;
 try
  if AValue = nil then Exit;
  
  Next:=FFirst;
  while Next <> nil do
   begin
    if Next = AValue then
     begin
      Result:=True;
      Exit;
     end;
     
    Next:=Next.Next;
   end;
 finally
  ReleaseLock;
 end;
end;

{==============================================================================}

procedure TWinsock2SocketBuffers.Clear; 
begin
 {}
 if not AcquireLock then Exit;

 FCount:=0;
 FFirst:=nil;
 FLast:=nil;

 ReleaseLock;
end;

{==============================================================================}
{==============================================================================}
{TWinsock2RAWSocket}
constructor TWinsock2RAWSocket.Create;
begin
 {}
 inherited Create;
 FSocketType:=SOCK_RAW;
 FProtocol:=IPPROTO_IP;
 
 FBufferSize:=WS2MaxDatagram;
 FBroadcastEnabled:=False;
end;
 
{==============================================================================}
 
procedure TWinsock2RAWSocket.SetSocketType(ASocketType:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Socket: SetSocketType');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=WSAEINVAL;
 case ASocketType of
  SOCK_RAW:begin
   FLastError:=ERROR_SUCCESS;
   FSocketType:=ASocketType;
   end;
 end;
end;

{==============================================================================}

procedure TWinsock2RAWSocket.SetProtocol(AProtocol:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Socket: SetProtocol');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 FProtocol:=AProtocol; {SOCK_RAW accepts any Protocol}
end;

{==============================================================================}

procedure TWinsock2RAWSocket.SetBufferSize(ABufferSize:Integer); 
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Socket: SetBufferSize');
 {$ENDIF}
 
 FLastError:=WSAEINVAL;
 if ABufferSize > WS2MaxDatagram then Exit;
 
 FLastError:=ERROR_SUCCESS;
 FBufferSize:=ABufferSize;
end;

{==============================================================================}

procedure TWinsock2RAWSocket.SetBroadcastEnabled(ABroadcastEnabled:Boolean);
var
 WorkBool:LongBool;
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Socket: SetBroadcastEnabled');
 {$ENDIF}
 
 FLastError:=ERROR_SUCCESS;
 FBroadcastEnabled:=ABroadcastEnabled;
 
 if not Connected then Exit;
 
 WorkBool:=FBroadcastEnabled;
 if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_BROADCAST,PChar(@WorkBool),SizeOf(WorkBool)) = ERROR_SUCCESS then Exit;

 FLastError:=Winsock2.WSAGetLastError;
 
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
 {$ENDIF}
end;

{==============================================================================}

function TWinsock2RAWSocket.RecvFromSocket(ASockAddr:PSockAddr;ASockLen:PInteger;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt;
var
 Closed:Boolean;
begin
 {}
 Closed:=False;
 Result:=RecvFromSocketEx(Handle,ASockAddr,ASockLen,AData,ASize,ACount,Closed);
end;

{==============================================================================}

function TWinsock2RAWSocket.SendToSocket(ASockAddr:PSockAddr;ASockLen:Integer;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt;
begin
 {}
 Result:=SendToSocketEx(Handle,ASockAddr,ASockLen,AData,ASize,ACount)
end;

{==============================================================================}

function TWinsock2RAWSocket.RecvFromSocketEx(AHandle:THandle;ASockAddr:PSockAddr;ASockLen:PInteger;AData:Pointer;ASize:Integer;var ACount:Integer;var AClosed:Boolean):LongInt;
var
 Count:Integer;
begin
 {}
 ACount:=0;
 AClosed:=False;
 Result:=SOCKET_ERROR;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAEINVAL;
 if AData = nil then Exit;

 if ASize > 0 then
  begin
   if ASockAddr = nil then
    begin
     Count:=Winsock2.recv(AHandle,AData^,ASize,0);
    end
   else
    begin
     Count:=Winsock2.recvfrom(AHandle,AData^,ASize,0,ASockAddr^,ASockLen^);
    end;
    
   if Count = SOCKET_ERROR then
    begin
     FLastError:=Winsock2.WSAGetLastError;
     Disconnect;
     Exit;
    end
   else
    begin
     if Count = 0 then
      begin
       FLastError:=ERROR_SUCCESS;
       AClosed:=True;
       Disconnect;
       Exit;
      end
     else
      begin
       ACount:=Count;
      end;
    end;
   
   Result:=ERROR_SUCCESS;
   FLastError:=ERROR_SUCCESS;
  end
 else
  begin
   Result:=ERROR_SUCCESS;
   FLastError:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function TWinsock2RAWSocket.SendToSocketEx(AHandle:THandle;ASockAddr:PSockAddr;ASockLen:Integer;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt;
var
 Count:Integer;
begin
 {}
 ACount:=0;
 Result:=SOCKET_ERROR;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAEINVAL;
 if AData = nil then Exit;

 if ASize > 0 then
  begin
   if ASockAddr = nil then
    begin
     Count:=Winsock2.send(Handle,AData^,ASize,0);
    end
   else
    begin
     Count:=Winsock2.sendto(Handle,AData^,ASize,0,ASockAddr^,ASockLen);
    end;    

   if Count = SOCKET_ERROR then
    begin
     FLastError:=Winsock2.WSAGetLastError;
     Disconnect;
     Exit;
    end
   else
    begin
     if Count = 0 then
      begin
       FLastError:=ERROR_SUCCESS;
       Disconnect;
       Exit;
      end
     else
      begin
       ACount:=Count;
      end;
    end;
   
   Result:=ERROR_SUCCESS;
   FLastError:=ERROR_SUCCESS;
  end
 else
  begin
   Result:=ERROR_SUCCESS;
   FLastError:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function TWinsock2RAWSocket.RecvData(AData:Pointer;ACount:Integer):Integer;
var
 Count:Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;
 
 if RecvFromSocket(nil,nil,AData,ACount,Count) = SOCKET_ERROR then Exit;
 
 FLastError:=ERROR_SUCCESS;
 Result:=Count;
end;

{==============================================================================}

function TWinsock2RAWSocket.SendData(AData:Pointer;ACount:Integer):Integer;
var
 Count:Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 if SendToSocket(nil,0,AData,ACount,Count) = SOCKET_ERROR then Exit;

 FLastError:=ERROR_SUCCESS;
 Result:=Count;
end;

{==============================================================================}

function TWinsock2RAWSocket.BroadcastData(AData:Pointer;ACount:Integer):Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 FLastError:=WSAEACCES;
 if not BroadcastEnabled then Exit;
 
 Result:=SendDataTo(GetBroadcastAddress,AData,ACount);
end;
 
{==============================================================================}

function TWinsock2RAWSocket.RecvDataFrom(var AHost:String;AData:Pointer;ACount:Integer):Integer;
var
 Count:Integer;
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
begin
 {}
 Result:=0;
 AHost:='';
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;
 
 {Allocate Address}
 SockAddrLength:=0;
 SockAddr:=AllocateAddress(SockAddrLength);
 if SockAddr = nil then Exit;
 try
  if RecvFromSocket(SockAddr,@SockAddrLength,AData,ACount,Count) = SOCKET_ERROR then Exit;
  
  AHost:=SockAddrToAddress(SockAddr,SockAddrLength);
  
  FLastError:=ERROR_SUCCESS;
  Result:=Count;
 finally
  ReleaseAddress(SockAddr,SockAddrLength);
 end;
end;

{==============================================================================}

function TWinsock2RAWSocket.SendDataTo(const AHost:String;AData:Pointer;ACount:Integer):Integer;
var
 Count:Integer;
 Address:String;
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 FLastError:=WSAEINVAL;
 if Length(AHost) = 0 then Exit;

 FLastError:=WSAEINVAL;
 Address:=ResolveHost(AHost);
 if Length(Address) = 0 then Exit;
 
 {Allocate Address}
 SockAddrLength:=0;
 SockAddr:=AddressToSockAddr(Address,SockAddrLength);
 if SockAddr = nil then Exit;
 try
  if SendToSocket(SockAddr,SockAddrLength,AData,ACount,Count) = SOCKET_ERROR then Exit;

  FLastError:=ERROR_SUCCESS;
  Result:=Count;
 finally
  ReleaseAddress(SockAddr,SockAddrLength);
 end; 
end;

{==============================================================================}

function TWinsock2RAWSocket.BroadcastDataTo(const AAddress:String;AData:Pointer;ACount:Integer):Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 FLastError:=WSAEACCES;
 if not BroadcastEnabled then Exit;
 
 Result:=SendDataTo(AAddress,AData,ACount);
end;

{==============================================================================}
{==============================================================================}
{TWinsock2TCPSocket}
constructor TWinsock2TCPSocket.Create;
begin
 {}
 inherited Create;
 FSocketType:=SOCK_STREAM;
 FProtocol:=IPPROTO_TCP;
 
 FBacklog:=SOMAXCONN;
 
 FUseNagle:=True;
 FUseKeepalive:=False;
 FSegmentSize:=8192;
 FMaxSegmentSize:=0;
end;

{==============================================================================}

procedure TWinsock2TCPSocket.SetSocketType(ASocketType:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket: SetSocketType');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=WSAEINVAL;
 case ASocketType of
  SOCK_STREAM:begin
   FLastError:=ERROR_SUCCESS;
   FSocketType:=ASocketType;
   end;
 end;
end;

{==============================================================================}

procedure TWinsock2TCPSocket.SetProtocol(AProtocol:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket: SetProtocol');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=WSAEINVAL;
 case AProtocol of
  IPPROTO_IP,IPPROTO_TCP:begin
   FLastError:=ERROR_SUCCESS;
   FProtocol:=AProtocol;
   end;
 end;
end;

{==============================================================================}

procedure TWinsock2TCPSocket.SetBacklog(ABacklog:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket: SetBacklog');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 FBacklog:=ABacklog;
end;

{==============================================================================}

procedure TWinsock2TCPSocket.SetUseNagle(AUseNagle:Boolean);
var
 WorkBool:LongBool;
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket: SetUseNagle');
 {$ENDIF}
 
 FLastError:=ERROR_SUCCESS;
 FUseNagle:=AUseNagle;
 
 if not Connected then Exit;
 
 WorkBool:=FUseNagle;
 if Winsock2.setsockopt(Handle,IPPROTO_TCP,TCP_NODELAY,PChar(@WorkBool),SizeOf(WorkBool)) = ERROR_SUCCESS then Exit;

 FLastError:=Winsock2.WSAGetLastError;
 
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
 {$ENDIF}
end;

{==============================================================================}

procedure TWinsock2TCPSocket.SetUseKeepalive(AUseKeepalive:Boolean);
var
 WorkBool:LongBool;
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket: SetUseKeepalive');
 {$ENDIF}
 
 FLastError:=ERROR_SUCCESS;
 FUseKeepalive:=AUseKeepalive;

 if not Connected then Exit;

 WorkBool:=FUseKeepalive;
 if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_KEEPALIVE,PChar(@WorkBool),SizeOf(WorkBool)) = ERROR_SUCCESS then Exit;

 FLastError:=Winsock2.WSAGetLastError;
 
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
 {$ENDIF}
end;

{==============================================================================}

procedure TWinsock2TCPSocket.SetSegmentSize(ASegmentSize:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket: SetSegmentSize');
 {$ENDIF}
 
 FLastError:=ERROR_SUCCESS;
 FSegmentSize:=ASegmentSize;
end;

{==============================================================================}

procedure TWinsock2TCPSocket.SetMaxSegmentSize(AMaxSegmentSize:LongWord);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket: SetMaxSegmentSize');
 {$ENDIF}
 
 FLastError:=ERROR_SUCCESS;
 FMaxSegmentSize:=AMaxSegmentSize;

 if not Connected then Exit;
 
 if Winsock2.setsockopt(Handle,IPPROTO_TCP,TCP_MAXSEG,PChar(@FMaxSegmentSize),SizeOf(FMaxSegmentSize)) = ERROR_SUCCESS then Exit;

 FLastError:=Winsock2.WSAGetLastError;
 
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
 {$ENDIF}
end;

{==============================================================================}

function TWinsock2TCPSocket.Listen(ABacklog:Integer):LongInt;
var
 WorkBool:LongBool;
begin
 {}
 Result:=SOCKET_ERROR;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket: Listen');
 {$ENDIF}
 
 {Set Options}
 if not UseNagle then
  begin
   WorkBool:=not UseNagle;
   if Winsock2.setsockopt(Handle,IPPROTO_TCP,TCP_NODELAY,PChar(@WorkBool),SizeOf(WorkBool)) = SOCKET_ERROR then
    begin
     FLastError:=Winsock2.WSAGetLastError;
      
     {$IFDEF WINSOCK2_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
     {$ENDIF}
      
     Exit;
    end;
  end;
 if UseKeepalive then
  begin
   WorkBool:=UseKeepalive;
   if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_KEEPALIVE,PChar(@WorkBool),SizeOf(WorkBool)) = SOCKET_ERROR then
    begin
     FLastError:=Winsock2.WSAGetLastError;
     
     {$IFDEF WINSOCK2_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
     {$ENDIF}
      
     Exit;
    end;
  end;
 if MaxSegmentSize <> 0 then
  begin
   if Winsock2.setsockopt(Handle,IPPROTO_TCP,TCP_MAXSEG,PChar(@MaxSegmentSize),SizeOf(MaxSegmentSize)) = SOCKET_ERROR then
    begin
     FLastError:=Winsock2.WSAGetLastError;
      
     {$IFDEF WINSOCK2_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
     {$ENDIF}
      
     Exit;
    end;
  end;
 
 {Set Backlog}
 FBacklog:=ABacklog;
 FLastError:=ERROR_SUCCESS;
 
 {Listen Socket}
 Result:=Winsock2.listen(Handle,Backlog);
 if Result = SOCKET_ERROR then
  begin
   FLastError:=Winsock2.WSAGetLastError;
   
   {$IFDEF WINSOCK2_DEBUG}
   if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Socket:  listen returned: = ' + Winsock2ErrorToString(FLastError));
   {$ENDIF}
  end;
end;

{==============================================================================}

function TWinsock2TCPSocket.ReadFromSocket(AData:Pointer;ASize:Integer):LongInt;
var
 Count:Integer;
 Closed:Boolean;
begin
 {}
 Count:=0;
 Closed:=False;
 Result:=ReadFromSocketEx(AData,ASize,Count,Closed,True,0);
end;

{==============================================================================}

function TWinsock2TCPSocket.WriteToSocket(AData:Pointer;ASize:Integer):LongInt;
var
 Count:Integer;
begin
 {}
 Count:=0;
 Result:=WriteToSocketEx(AData,ASize,Count,True,0);
end;

{==============================================================================}

function TWinsock2TCPSocket.ReadFromSocketEx(AData:Pointer;ASize:Integer;var ACount:Integer;var AClosed:Boolean;AWait:Boolean;ATimeout:Integer):LongInt;
var
 Count:Integer;
 Offset:Integer;
 StartTime:Int64;
 CurrentTime:Int64;
begin
 {}
 ACount:=0;
 AClosed:=False;
 Result:=SOCKET_ERROR;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAEINVAL;
 if AData = nil then Exit;

 if ASize > 0 then
  begin
   Offset:=0;
   StartTime:=GetTickCount64;
   repeat
    Count:=Winsock2.recv(Handle,Pointer(PtrUInt(AData) + PtrUInt(Offset))^,ASize - Offset,0);
    if Count = SOCKET_ERROR then
     begin
      FLastError:=Winsock2.WSAGetLastError;
      Disconnect;
      Exit;
     end
    else
     begin
      if Count = 0 then
       begin
        FLastError:=ERROR_SUCCESS;
        AClosed:=True;
        Disconnect;
        Exit;
       end
      else
       begin
        Inc(ACount,Count);
        Inc(Offset,Count);
       end;
     end;
    if not AWait then Break;
    if ATimeout <> 0 then
     begin
      CurrentTime:=GetTickCount64;
      if CurrentTime > (StartTime + ATimeout) then Break;
     end;
   until Offset >= ASize;
   Result:=ERROR_SUCCESS;
   FLastError:=ERROR_SUCCESS;
  end
 else
  begin
   Result:=ERROR_SUCCESS;
   FLastError:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function TWinsock2TCPSocket.WriteToSocketEx(AData:Pointer;ASize:Integer;var ACount:Integer;AWait:Boolean;ATimeout:Integer):LongInt;
var
 Count:Integer;
 Offset:Integer;
 StartTime:Int64;
 CurrentTime:Int64;
begin
 {}
 ACount:=0;
 Result:=SOCKET_ERROR;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAEINVAL;
 if AData = nil then Exit;

 if ASize > 0 then
  begin
   Offset:=0;
   StartTime:=GetTickCount64;
   repeat
    Count:=Winsock2.send(Handle,Pointer(PtrUInt(AData) + PtrUInt(Offset))^,ASize - Offset,0);
    if Count = SOCKET_ERROR then
     begin
      FLastError:=Winsock2.WSAGetLastError;
      Disconnect;
      Exit;
     end
    else
     begin
      if Count = 0 then
       begin
        FLastError:=ERROR_SUCCESS;
        Disconnect;
        Exit;
       end
      else
       begin
        Inc(ACount,Count);
        Inc(Offset,Count);
       end;
     end;
    if not AWait then Break;
    if ATimeout <> 0 then
     begin
      CurrentTime:=GetTickCount64;
      if CurrentTime > (StartTime + ATimeout) then Break;
     end;
   until Offset >= ASize;
   Result:=ERROR_SUCCESS;
   FLastError:=ERROR_SUCCESS;
  end
 else
  begin
   Result:=ERROR_SUCCESS;
   FLastError:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function TWinsock2TCPSocket.ReadData(AData:Pointer;ACount:Integer):Boolean;
var
 Size:Integer;
 Offset:Integer;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 Offset:=0;
 while ACount > 0 do
  begin
   if ACount > FSegmentSize then
    begin
     Size:=FSegmentSize;
    end
   else
    begin
     Size:=ACount;
    end;
   if ReadFromSocket(Pointer(PtrUInt(AData) + PtrUInt(Offset)),Size) = SOCKET_ERROR then Exit;
   Dec(ACount,Size);
   Inc(Offset,Size);
  end;
 FLastError:=ERROR_SUCCESS;
 Result:=True;
end;

{==============================================================================}

function TWinsock2TCPSocket.WriteData(AData:Pointer;ACount:Integer):Boolean;
var
 Size:Integer;
 Offset:Integer;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 Offset:=0;
 while ACount > 0 do
  begin
   if ACount > FSegmentSize then
    begin
     Size:=FSegmentSize;
    end
   else
    begin
     Size:=ACount;
    end;
   if WriteToSocket(Pointer(PtrUInt(AData) + PtrUInt(Offset)),Size) = SOCKET_ERROR then Exit;
   Dec(ACount,Size);
   Inc(Offset,Size);
  end;
 FLastError:=ERROR_SUCCESS;
 Result:=True;
end;

{==============================================================================}

function TWinsock2TCPSocket.ReadAvailable(AData:Pointer;ASize:Integer;var ACount:Integer;var AClosed:Boolean):Boolean;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 if ReadFromSocketEx(AData,ASize,ACount,AClosed,False,0) = SOCKET_ERROR then Exit;

 FLastError:=ERROR_SUCCESS;
 Result:=True;
end;
  
{==============================================================================}
{==============================================================================}
{TWinsock2UDPSocket}
constructor TWinsock2UDPSocket.Create;
begin
 {}
 inherited Create;
 FSocketType:=SOCK_DGRAM;
 FProtocol:=IPPROTO_UDP;
 
 FBufferSize:=WS2MaxDatagram;
 FBroadcastEnabled:=False;
end;

{==============================================================================}

procedure TWinsock2UDPSocket.SetSocketType(ASocketType:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Socket: SetSocketType');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=WSAEINVAL;
 case ASocketType of
  SOCK_DGRAM:begin
   FLastError:=ERROR_SUCCESS;
   FSocketType:=ASocketType;
   end;
 end;
end;

{==============================================================================}

procedure TWinsock2UDPSocket.SetProtocol(AProtocol:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Socket: SetProtocol');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=WSAEINVAL;
 case AProtocol of
  IPPROTO_IP,IPPROTO_UDP:begin
   FLastError:=ERROR_SUCCESS;
   FProtocol:=AProtocol;
   end;
 end;
end;

{==============================================================================}

procedure TWinsock2UDPSocket.SetBufferSize(ABufferSize:Integer); 
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Socket: SetBufferSize');
 {$ENDIF}
 
 FLastError:=WSAEINVAL;
 if ABufferSize > WS2MaxDatagram then Exit;
 
 FLastError:=ERROR_SUCCESS;
 FBufferSize:=ABufferSize;
end;

{==============================================================================}

procedure TWinsock2UDPSocket.SetBroadcastEnabled(ABroadcastEnabled:Boolean);
var
 WorkBool:LongBool;
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Socket: SetBroadcastEnabled');
 {$ENDIF}
 
 FLastError:=ERROR_SUCCESS;
 FBroadcastEnabled:=ABroadcastEnabled;
 
 if not Connected then Exit;
 
 WorkBool:=FBroadcastEnabled;
 if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_BROADCAST,PChar(@WorkBool),SizeOf(WorkBool)) = ERROR_SUCCESS then Exit;

 FLastError:=Winsock2.WSAGetLastError;
 
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Socket:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
 {$ENDIF}
end;

{==============================================================================}

function TWinsock2UDPSocket.RecvFromSocket(ASockAddr:PSockAddr;ASockLen:PInteger;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt;
var
 Closed:Boolean;
begin
 {}
 Closed:=False;
 Result:=RecvFromSocketEx(Handle,ASockAddr,ASockLen,AData,ASize,ACount,Closed);
end;

{==============================================================================}

function TWinsock2UDPSocket.SendToSocket(ASockAddr:PSockAddr;ASockLen:Integer;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt;
begin
 {}
 Result:=SendToSocketEx(Handle,ASockAddr,ASockLen,AData,ASize,ACount);
end;

{==============================================================================}

function TWinsock2UDPSocket.RecvFromSocketEx(AHandle:THandle;ASockAddr:PSockAddr;ASockLen:PInteger;AData:Pointer;ASize:Integer;var ACount:Integer;var AClosed:Boolean):LongInt;
var
 Count:Integer;
begin
 {}
 ACount:=0;
 AClosed:=False;
 Result:=SOCKET_ERROR;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAEINVAL;
 if AData = nil then Exit;

 if ASize > 0 then
  begin
   if ASockAddr = nil then
    begin
     Count:=Winsock2.recv(AHandle,AData^,ASize,0);
    end
   else
    begin
     Count:=Winsock2.recvfrom(AHandle,AData^,ASize,0,ASockAddr^,ASockLen^);
    end;
    
   if Count = SOCKET_ERROR then
    begin
     FLastError:=Winsock2.WSAGetLastError;
     Disconnect;
     Exit;
    end
   else
    begin
     if Count = 0 then
      begin
       FLastError:=ERROR_SUCCESS;
       AClosed:=True;
       Disconnect;
       Exit;
      end
     else
      begin
       ACount:=Count;
      end;
    end;
   
   Result:=ERROR_SUCCESS;
   FLastError:=ERROR_SUCCESS;
  end
 else
  begin
   Result:=ERROR_SUCCESS;
   FLastError:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function TWinsock2UDPSocket.SendToSocketEx(AHandle:THandle;ASockAddr:PSockAddr;ASockLen:Integer;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt;
var
 Count:Integer;
begin
 {}
 ACount:=0;
 Result:=SOCKET_ERROR;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAEINVAL;
 if AData = nil then Exit;

 if ASize > 0 then
  begin
   if ASockAddr = nil then
    begin
     Count:=Winsock2.send(Handle,AData^,ASize,0);
    end
   else
    begin
     Count:=Winsock2.sendto(Handle,AData^,ASize,0,ASockAddr^,ASockLen);
    end;    

   if Count = SOCKET_ERROR then
    begin
     FLastError:=Winsock2.WSAGetLastError;
     Disconnect;
     Exit;
    end
   else
    begin
     if Count = 0 then
      begin
       FLastError:=ERROR_SUCCESS;
       Disconnect;
       Exit;
      end
     else
      begin
       ACount:=Count;
      end;
    end;
   
   Result:=ERROR_SUCCESS;
   FLastError:=ERROR_SUCCESS;
  end
 else
  begin
   Result:=ERROR_SUCCESS;
   FLastError:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function TWinsock2UDPSocket.RecvData(AData:Pointer;ACount:Integer):Integer;
var
 Count:Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;
 
 if RecvFromSocket(nil,nil,AData,ACount,Count) = SOCKET_ERROR then Exit;
 
 FLastError:=ERROR_SUCCESS;
 Result:=Count;
end;

{==============================================================================}

function TWinsock2UDPSocket.SendData(AData:Pointer;ACount:Integer):Integer;
var
 Count:Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 if SendToSocket(nil,0,AData,ACount,Count) = SOCKET_ERROR then Exit;

 FLastError:=ERROR_SUCCESS;
 Result:=Count;
end;

{==============================================================================}

function TWinsock2UDPSocket.BroadcastData(APort:Word;AData:Pointer;ACount:Integer):Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 FLastError:=WSAEACCES;
 if not BroadcastEnabled then Exit;
 
 Result:=SendDataTo(GetBroadcastAddress,APort,AData,ACount);
end;

{==============================================================================}

function TWinsock2UDPSocket.RecvDataFrom(var AHost:String;var APort:Word;AData:Pointer;ACount:Integer):Integer;
var
 Count:Integer;
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
begin
 {}
 Result:=0;
 AHost:='';
 APort:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;
 
 {Allocate Address}
 SockAddrLength:=0;
 SockAddr:=AllocateAddress(SockAddrLength);
 if SockAddr = nil then Exit;
 try
  if RecvFromSocket(SockAddr,@SockAddrLength,AData,ACount,Count) = SOCKET_ERROR then Exit;
  
  AHost:=SockAddrToAddress(SockAddr,SockAddrLength);
  APort:=SockAddrToPort(SockAddr,SockAddrLength);
  
  FLastError:=ERROR_SUCCESS;
  Result:=Count;
 finally
  ReleaseAddress(SockAddr,SockAddrLength);
 end;
end;

{==============================================================================}

function TWinsock2UDPSocket.SendDataTo(const AHost:String;APort:Word;AData:Pointer;ACount:Integer):Integer;
var
 Count:Integer;
 Address:String;
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 FLastError:=WSAEINVAL;
 if Length(AHost) = 0 then Exit;
 if APort = 0 then Exit;
 
 FLastError:=WSAEINVAL;
 Address:=ResolveHost(AHost);
 if Length(Address) = 0 then Exit;
 
 {Allocate Address}
 SockAddrLength:=0;
 SockAddr:=AddressToSockAddr(Address,SockAddrLength);
 if SockAddr = nil then Exit;
 try
  FLastError:=WSAEINVAL;
  if not PortToSockAddr(APort,SockAddr,SockAddrLength) then Exit;
  
  if SendToSocket(SockAddr,SockAddrLength,AData,ACount,Count) = SOCKET_ERROR then Exit;

  FLastError:=ERROR_SUCCESS;
  Result:=Count;
 finally
  ReleaseAddress(SockAddr,SockAddrLength);
 end; 
end;

{==============================================================================}

function TWinsock2UDPSocket.BroadcastDataTo(const AAddress:String;APort:Word;AData:Pointer;ACount:Integer):Integer;
begin
 {}
 Result:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FLastError:=WSAENOTCONN;
 if not Connected then Exit;

 FLastError:=WSAEACCES;
 if not BroadcastEnabled then Exit;
 
 Result:=SendDataTo(AAddress,APort,AData,ACount);
end;

{==============================================================================}
{==============================================================================}
{TWinsock2RAWClient}
constructor TWinsock2RAWClient.Create;
begin
 {}
 inherited Create;
 FUseConnect:=True;
 
 FRemoteHost:='';
 FRemoteAddress:='';
end;

{==============================================================================}

destructor TWinsock2RAWClient.Destroy;
begin
 {}
 if Connected then Disconnect;
 inherited Destroy;
end;

{==============================================================================}

procedure TWinsock2RAWClient.SetUseConnect(AUseConnect:Boolean);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client: SetUseConnect');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 FUseConnect:=AUseConnect;
end;

{==============================================================================}

procedure TWinsock2RAWClient.SetFamily(AFamily:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client: SetFamily');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=WSAEINVAL;
 case AFamily of
  AF_UNSPEC,AF_INET,AF_INET6:begin
    {Unspecified, IPv4 or IPv6}
    FLastError:=ERROR_SUCCESS;
    if FFamily <> AFamily then FBoundAddress:='';
    if FFamily <> AFamily then FRemoteAddress:='';
    FFamily:=AFamily;
   end;
 end;
end;

{==============================================================================}

procedure TWinsock2RAWClient.SetBoundAddress(const ABoundAddress:String);
var
 WorkInt:Integer;
 WorkBuffer:String;
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client: SetBoundAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  BoundAddress = ' + ABoundAddress);
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 {Set Bound Address}
 FLastError:=ERROR_SUCCESS;
 WorkBuffer:=RemoveBraces(ABoundAddress);
 if FBoundAddress = WorkBuffer then Exit;
 FBoundAddress:=WorkBuffer;
 {Check Bound Address}
 if Length(FBoundAddress) = 0 then
  begin
   {Check Remote Address}
   if Length(FRemoteAddress) = 0 then
    begin
     FFamily:=AF_UNSPEC;
    end;
  end
 else
  begin
   {Check Remote Address}
   if Length(FRemoteAddress) = 0 then
    begin
     FFamily:=ResolveFamily(FBoundAddress);
    end
   else
    begin
     {Set Family}
     FFamily:=ResolveFamily(FRemoteAddress);
     {Compare Family}
     WorkInt:=ResolveFamily(FBoundAddress);
     if WorkInt <> FFamily then
      begin
       FBoundAddress:='';
      end;
    end;
  end;
end;

{==============================================================================}

procedure TWinsock2RAWClient.SetRemoteHost(const ARemoteHost:String);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client: SetRemoteHost');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  RemoteHost = ' + ARemoteHost);
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 {Set Remote Host}
 FLastError:=ERROR_SUCCESS;
 FRemoteHost:=ARemoteHost;
 {Check Remote Host}
 if Length(FRemoteHost) <> 0 then
  begin
   {Set Remote Address}
   FRemoteAddress:='';
   {Check Bound Address}
   if Length(FBoundAddress) = 0 then
    begin
     FFamily:=AF_UNSPEC;
    end
   else
    begin
     FFamily:=ResolveFamily(FBoundAddress);
    end;
  end
end;

{==============================================================================}

procedure TWinsock2RAWClient.SetRemoteAddress(const ARemoteAddress:String);
var
 WorkInt:Integer;
 WorkBuffer:String;
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client: SetRemoteAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  RemoteAddress = ' + ARemoteAddress);
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 {Set Remote Address}
 FLastError:=ERROR_SUCCESS;
 WorkBuffer:=RemoveBraces(ARemoteAddress);
 if FRemoteAddress = WorkBuffer then Exit;
 FRemoteAddress:=WorkBuffer;
 {Check Remote Address}
 if Length(FRemoteAddress) = 0 then
  begin
   {Check Bound Address}
   if Length(FBoundAddress) = 0 then
    begin
     FFamily:=AF_UNSPEC;
    end;
  end
 else
  begin
   {Set Remote Host}
   FRemoteHost:='';
   {Check Bound Address}
   if Length(FBoundAddress) = 0 then
    begin
     FFamily:=ResolveFamily(FRemoteAddress);
    end
   else
    begin
     {Set Family}
     FFamily:=ResolveFamily(FRemoteAddress);
     {Compare Family}
     WorkInt:=ResolveFamily(FBoundAddress);
     if WorkInt <> FFamily then
      begin
       FBoundAddress:='';
      end;
    end;
  end;
end;

{==============================================================================}

function TWinsock2RAWClient.AllocateFamily:Boolean;
{Check Family, Bound Address and Remote Address, normalise as needed}
var
 WorkInt:Integer;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client: AllocateFamily');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=WSAEINVAL;
 {Check Remote Address}
 if Length(FRemoteAddress) = 0 then
  begin
   Result:=inherited AllocateFamily;
  end
 else 
  begin
   {Check Family}
   case FFamily of
    AF_UNSPEC:begin
      {Unspecified}
      FFamily:=ResolveFamily(FRemoteAddress);
     end;
    AF_INET,AF_INET6:begin
      {IPv4 or IPv6}
      WorkInt:=ResolveFamily(FRemoteAddress);
      if WorkInt <> FFamily then
       begin
        FFamily:=WorkInt;
       end;
     end;
   end;
   {Check Bound Address}
   if Length(FBoundAddress) = 0 then
    begin
     {Check Family}
     case FFamily of
      AF_INET:begin
        {IPv4}
        FBoundAddress:=INET_ADDRSTR_ANY;
        FLastError:=ERROR_SUCCESS;
        Result:=True;
       end;
      AF_INET6:begin
        {IPv6}
        FBoundAddress:=INET6_ADDRSTR_INIT;
        FLastError:=ERROR_SUCCESS;
        Result:=True;
       end;
     end;
    end
   else
    begin
     {Check Family}
     case FFamily of
      AF_INET:begin
        {IPv4}
        WorkInt:=ResolveFamily(FBoundAddress);
        if WorkInt <> FFamily then
         begin
          FBoundAddress:=INET_ADDRSTR_ANY;
         end;
        FLastError:=ERROR_SUCCESS;
        Result:=True;
       end;
      AF_INET6:begin
        {IPv6}
        WorkInt:=ResolveFamily(FBoundAddress);
        if WorkInt <> FFamily then
         begin
          FBoundAddress:=INET6_ADDRSTR_INIT;
         end;
        FLastError:=ERROR_SUCCESS;
        Result:=True;
       end;
     end;
    end;
  end;
  
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  RemoteAddress = ' + FRemoteAddress);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  BoundAddress = ' + FBoundAddress);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  Family = ' + AddressFamilyToString(FFamily));
 {$ENDIF}
end;

{==============================================================================}

function TWinsock2RAWClient.AllocateRemoteAddress(var ALength:Integer):PSockAddr;
{Allocate a socket address structure for the remote address and port}
{Note: Change this to use InetPton for both IPv4 and IPv6 addresses}
var
 InAddr:TInAddr;
 SockAddr:TSockAddrIn6;
 {BufferLength:LongInt;}
begin
 {}
 Result:=nil;
 ALength:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client: AllocateRemoteAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  RemoteAddress = ' + FRemoteAddress);
 {$ENDIF}
 
 FLastError:=WSAEINVAL;
 if Length(FRemoteAddress) = 0 then Exit;

 {Allocate Address}
 Result:=AllocateAddress(ALength);
 if Result = nil then Exit;

 {Check Family}
 case Result.sin_family of
  AF_INET:begin
    {IPv4}
    {Setup remote address}
    Result.sin_port:=Winsock2.htons(IPPORT_ANY);
    InAddr.S_addr:=Winsock2.inet_addr(PChar(FRemoteAddress));
    if (InAddr.S_addr = INADDR_NONE) and (FRemoteAddress <> INET_ADDRSTR_BROADCAST) then
     begin
      FLastError:=WSAEINVAL;
      ReleaseAddress(Result,ALength);

      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  inet_addr returned: = INADDR_NONE');
      {$ENDIF}
      
      Exit;
     end;
    if InAddr.S_addr = INADDR_ANY then
     begin
      FLastError:=WSAEINVAL;
      ReleaseAddress(Result,ALength);

      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  inet_addr returned: = INADDR_ANY');
      {$ENDIF}

      Exit;
     end;
    Result.sin_addr.S_addr:=InAddr.S_addr;
    FLastError:=ERROR_SUCCESS;
   end;
  AF_INET6:begin
    {IPv6}
    {Setup remote address}
    PSockAddrIn6(Result).sin6_port:=Winsock2.htons(IPPORT_ANY);
    
    if Winsock2.InetPtonA(AF_INET6,PChar(FRemoteAddress),@SockAddr.sin6_addr) = SOCKET_ERROR then
     begin
      FLastError:=Winsock2.WSAGetLastError;
      ReleaseAddress(Result,ALength);
 
      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  InetPtonA returned: ' + Winsock2ErrorToString(FLastError));
      {$ENDIF}
      
      Exit;
     end;
    
    {BufferLength:=SizeOf(TSockAddrIn6);
    if Winsock2.WSAStringToAddressA(PChar(FRemoteAddress),FFamily,nil,PSockAddrIn(@SockAddr)^,BufferLength) = SOCKET_ERROR then
     begin
      FLastError:=Winsock2.WSAGetLastError;
      ReleaseAddress(Result,ALength);
      Exit;
     end;}
     
    PSockAddrIn6(Result).sin6_addr.s6_addr:=SockAddr.sin6_addr.s6_addr;
    FLastError:=ERROR_SUCCESS;
   end;
 end;
end;

{==============================================================================}

function TWinsock2RAWClient.Connect:Boolean;
var
 WorkInt:Integer;
 WorkBool:LongBool;
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client: Connect');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  RemoteHost = ' + FRemoteHost);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  RemoteAddress = ' + FRemoteAddress);
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 {Check Remote Host}
 if Length(FRemoteHost) <> 0 then
  begin
   WorkInt:=FFamily;
   try
    if FFamily = AF_UNSPEC then FFamily:=AF_INET;
    FRemoteAddress:=ResolveHost(FRemoteHost);
    if Length(FRemoteAddress) = 0 then
     begin
      if FFamily = AF_INET then FFamily:=AF_INET6 else FFamily:=AF_INET;
      FRemoteAddress:=ResolveHost(FRemoteHost);
     end;
     
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  ResolvedAddress = ' + FRemoteAddress);
    {$ENDIF}
   finally
    FFamily:=WorkInt;
   end;
  end;

 {Check Family}
 if not AllocateFamily then Exit;

 {Create Socket}
 if AllocateSocket(FSocketType) = INVALID_SOCKET then Exit;
 try
  {Set Options}
  if BroadcastEnabled then
   begin
    WorkBool:=BroadcastEnabled;
    if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_BROADCAST,PChar(@WorkBool),SizeOf(WorkBool)) = SOCKET_ERROR then
     begin
      FLastError:=Winsock2.WSAGetLastError;

      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
      {$ENDIF}

      Exit;
     end;
   end;
  
  {Bind Socket}
  if Bind = SOCKET_ERROR then Exit;
  
  {Connect Socket}
  if UseConnect then
   begin
    SockAddrLength:=0;
    SockAddr:=AllocateRemoteAddress(SockAddrLength);
    if SockAddr = nil then Exit;
    try
     FLastError:=ERROR_SUCCESS;
     if Winsock2.connect(Handle,SockAddr^,SockAddrLength) = SOCKET_ERROR then
      begin
       FLastError:=Winsock2.WSAGetLastError;
       
       {$IFDEF WINSOCK2_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client:  connect returned: = ' + Winsock2ErrorToString(FLastError));
       {$ENDIF}
       
       Exit;
      end;
     Result:=True;
    finally
     ReleaseAddress(SockAddr,SockAddrLength);
    end;
   end
  else
   begin
    FLastError:=ERROR_SUCCESS;
    Result:=True;
   end;   
 finally
  if not(Result) then Disconnect;
 end;
end;

{==============================================================================}

function TWinsock2RAWClient.Disconnect:Boolean;
begin
 {}
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 RAW Client: Disconnect');
 {$ENDIF}

 if Length(FRemoteHost) <> 0 then FRemoteAddress:='';
 Result:=inherited Disconnect;
end;

{==============================================================================}
{==============================================================================}
{TWinsock2TCPClient}
constructor TWinsock2TCPClient.Create;
begin
 {}
 inherited Create;
 FRemotePort:=0;
 FRemoteHost:='';
 FRemoteAddress:='';
end;

{==============================================================================}

destructor TWinsock2TCPClient.Destroy;
begin
 {}
 if Connected then Disconnect;
 inherited Destroy;
end;

{==============================================================================}

procedure TWinsock2TCPClient.SetFamily(AFamily:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client: SetFamily');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=WSAEINVAL;
 case AFamily of
  AF_UNSPEC,AF_INET,AF_INET6:begin
    {Unspecified, IPv4 or IPv6}
    FLastError:=ERROR_SUCCESS;
    if FFamily <> AFamily then FBoundAddress:='';
    if FFamily <> AFamily then FRemoteAddress:='';
    FFamily:=AFamily;
   end;
 end;
end;

{==============================================================================}

procedure TWinsock2TCPClient.SetBoundAddress(const ABoundAddress:String);
var
 WorkInt:Integer;
 WorkBuffer:String;
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client: SetBoundAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  BoundAddress = ' + ABoundAddress);
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 {Set Bound Address}
 FLastError:=ERROR_SUCCESS;
 WorkBuffer:=RemoveBraces(ABoundAddress);
 if FBoundAddress = WorkBuffer then Exit;
 FBoundAddress:=WorkBuffer;
 {Check Bound Address}
 if Length(FBoundAddress) = 0 then
  begin
   {Check Remote Address}
   if Length(FRemoteAddress) = 0 then
    begin
     FFamily:=AF_UNSPEC;
    end;
  end
 else
  begin
   {Check Remote Address}
   if Length(FRemoteAddress) = 0 then
    begin
     FFamily:=ResolveFamily(FBoundAddress);
    end
   else
    begin
     {Set Family}
     FFamily:=ResolveFamily(FRemoteAddress);
     {Compare Family}
     WorkInt:=ResolveFamily(FBoundAddress);
     if WorkInt <> FFamily then
      begin
       FBoundAddress:='';
      end;
    end;
  end;
end;

{==============================================================================}

procedure TWinsock2TCPClient.SetRemotePort(ARemotePort:Word);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client: SetRemotePort');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  RemotePort = ' + IntToStr(ARemotePort));
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 FRemotePort:=ARemotePort;
end;

{==============================================================================}

procedure TWinsock2TCPClient.SetRemoteHost(const ARemoteHost:String);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client: SetRemoteHost');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  RemoteHost = ' + ARemoteHost);
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 {Set Remote Host}
 FLastError:=ERROR_SUCCESS;
 FRemoteHost:=ARemoteHost;
 {Check Remote Host}
 if Length(FRemoteHost) <> 0 then
  begin
   {Set Remote Address}
   FRemoteAddress:='';
   {Check Bound Address}
   if Length(FBoundAddress) = 0 then
    begin
     FFamily:=AF_UNSPEC;
    end
   else
    begin
     FFamily:=ResolveFamily(FBoundAddress);
    end;
  end
end;

{==============================================================================}

procedure TWinsock2TCPClient.SetRemoteAddress(const ARemoteAddress:String);
var
 WorkInt:Integer;
 WorkBuffer:String;
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client: SetRemoteAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  RemoteAddress = ' + ARemoteAddress);
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 {Set Remote Address}
 FLastError:=ERROR_SUCCESS;
 WorkBuffer:=RemoveBraces(ARemoteAddress);
 if FRemoteAddress = WorkBuffer then Exit;
 FRemoteAddress:=WorkBuffer;
 {Check Remote Address}
 if Length(FRemoteAddress) = 0 then
  begin
   {Check Bound Address}
   if Length(FBoundAddress) = 0 then
    begin
     FFamily:=AF_UNSPEC;
    end;
  end
 else
  begin
   {Set Remote Host}
   FRemoteHost:='';
   {Check Bound Address}
   if Length(FBoundAddress) = 0 then
    begin
     FFamily:=ResolveFamily(FRemoteAddress);
    end
   else
    begin
     {Set Family}
     FFamily:=ResolveFamily(FRemoteAddress);
     {Compare Family}
     WorkInt:=ResolveFamily(FBoundAddress);
     if WorkInt <> FFamily then
      begin
       FBoundAddress:='';
      end;
    end;
  end;
end;

{==============================================================================}

function TWinsock2TCPClient.AllocateFamily:Boolean;
{Check Family, Bound Address and Remote Address, normalise as needed}
var
 WorkInt:Integer;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client: AllocateFamily');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=WSAEINVAL;
 {Check Remote Address}
 if Length(FRemoteAddress) <> 0 then
  begin
   {Check Family}
   case FFamily of
    AF_UNSPEC:begin
      {Unspecified}
      FFamily:=ResolveFamily(FRemoteAddress);
     end;
    AF_INET,AF_INET6:begin
      {IPv4 or IPv6}
      WorkInt:=ResolveFamily(FRemoteAddress);
      if WorkInt <> FFamily then
       begin
        FFamily:=WorkInt;
       end;
     end;
   end;
   {Check Bound Address}
   if Length(FBoundAddress) = 0 then
    begin
     {Check Family}
     case FFamily of
      AF_INET:begin
        {IPv4}
        FBoundAddress:=INET_ADDRSTR_ANY;
        FLastError:=ERROR_SUCCESS;
        Result:=True;
       end;
      AF_INET6:begin
        {IPv6}
        FBoundAddress:=INET6_ADDRSTR_INIT;
        FLastError:=ERROR_SUCCESS;
        Result:=True;
       end;
     end;
    end
   else
    begin
     {Check Family}
     case FFamily of
      AF_INET:begin
        {IPv4}
        WorkInt:=ResolveFamily(FBoundAddress);
        if WorkInt <> FFamily then
         begin
          FBoundAddress:=INET_ADDRSTR_ANY;
         end;
        FLastError:=ERROR_SUCCESS;
        Result:=True;
       end;
      AF_INET6:begin
        {IPv6}
        WorkInt:=ResolveFamily(FBoundAddress);
        if WorkInt <> FFamily then
         begin
          FBoundAddress:=INET6_ADDRSTR_INIT;
         end;
        FLastError:=ERROR_SUCCESS;
        Result:=True;
       end;
     end;
    end;
  end;
  
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  RemoteAddress = ' + FRemoteAddress);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  BoundAddress = ' + FBoundAddress);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  Family = ' + AddressFamilyToString(FFamily));
 {$ENDIF}
end;

{==============================================================================}

function TWinsock2TCPClient.AllocateRemoteAddress(var ALength:Integer):PSockAddr;
{Allocate a socket address structure for the remote address and port}
{Note: Change this to use InetPton for both IPv4 and IPv6 addresses}
var
 InAddr:TInAddr;
 SockAddr:TSockAddrIn6;
 {BufferLength:LongInt;}
begin
 {}
 Result:=nil;
 ALength:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client: AllocateRemoteAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  RemoteAddress = ' + FRemoteAddress);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  RemotePort = ' + IntToStr(FRemotePort));
 {$ENDIF}
 
 FLastError:=WSAEINVAL;
 if Length(FRemoteAddress) = 0 then Exit;

 {Allocate Address}
 Result:=AllocateAddress(ALength);
 if Result = nil then Exit;

 {Check Family}
 case Result.sin_family of
  AF_INET:begin
    {IPv4}
    {Setup remote address}
    Result.sin_port:=Winsock2.htons(FRemotePort);
    InAddr.S_addr:=Winsock2.inet_addr(PChar(FRemoteAddress));
    if InAddr.S_addr = INADDR_NONE then
     begin
      FLastError:=WSAEINVAL;
      ReleaseAddress(Result,ALength);
      
      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  inet_addr returned: = INADDR_NONE');
      {$ENDIF}
      
      Exit;
     end;
    if InAddr.S_addr = INADDR_ANY then
     begin
      FLastError:=WSAEINVAL;
      ReleaseAddress(Result,ALength);
      
      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  inet_addr returned: = INADDR_ANY');
      {$ENDIF}
      
      Exit;
     end;
    Result.sin_addr.S_addr:=InAddr.S_addr;
    FLastError:=ERROR_SUCCESS;
   end;
  AF_INET6:begin
    {IPv6}
    {Setup remote address}
    PSockAddrIn6(Result).sin6_port:=Winsock2.htons(FRemotePort);
    
    if Winsock2.InetPtonA(AF_INET6,PChar(FRemoteAddress),@SockAddr.sin6_addr) = SOCKET_ERROR then
     begin
      FLastError:=Winsock2.WSAGetLastError;
      ReleaseAddress(Result,ALength);
      
      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  InetPtonA returned: ' + Winsock2ErrorToString(FLastError));
      {$ENDIF}
      
      Exit;
     end;
    
    {BufferLength:=SizeOf(TSockAddrIn6);
    if Winsock2.WSAStringToAddressA(PChar(FRemoteAddress),FFamily,nil,PSockAddrIn(@SockAddr)^,BufferLength) = SOCKET_ERROR then
     begin
      FLastError:=Winsock2.WSAGetLastError;
      ReleaseAddress(Result,ALength);
      Exit;
     end;}
     
    PSockAddrIn6(Result).sin6_addr.s6_addr:=SockAddr.sin6_addr.s6_addr;
    FLastError:=ERROR_SUCCESS;
   end;
 end;
end;

{==============================================================================}

function TWinsock2TCPClient.Connect:Boolean;
var
 WorkInt:Integer;
 WorkBool:LongBool;
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client: Connect');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  RemoteHost = ' + FRemoteHost);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  RemoteAddress = ' + FRemoteAddress);
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 {Check Remote Host}
 if Length(FRemoteHost) <> 0 then
  begin
   WorkInt:=FFamily;
   try
    if FFamily = AF_UNSPEC then FFamily:=AF_INET;
    FRemoteAddress:=ResolveHost(FRemoteHost);
    if Length(FRemoteAddress) = 0 then
     begin
      if FFamily = AF_INET then FFamily:=AF_INET6 else FFamily:=AF_INET;
      FRemoteAddress:=ResolveHost(FRemoteHost);
     end;
     
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  ResolvedAddress = ' + FRemoteAddress);
    {$ENDIF}
   finally
    FFamily:=WorkInt;
   end;
  end;

 {Check Family}
 if not AllocateFamily then Exit;

 {Create Socket}
 if AllocateSocket(FSocketType) = INVALID_SOCKET then Exit;
 try
  {Set Options}
  if not UseNagle then
   begin
    WorkBool:=not UseNagle;
    if Winsock2.setsockopt(Handle,IPPROTO_TCP,TCP_NODELAY,PChar(@WorkBool),SizeOf(WorkBool)) = SOCKET_ERROR then
     begin
      FLastError:=Winsock2.WSAGetLastError;
      
      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
      {$ENDIF}
      
      Exit;
     end;
   end;
  if UseKeepalive then
   begin
    WorkBool:=UseKeepalive;
    if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_KEEPALIVE,PChar(@WorkBool),SizeOf(WorkBool)) = SOCKET_ERROR then
     begin
      FLastError:=Winsock2.WSAGetLastError;
      
      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
      {$ENDIF}
      
      Exit;
     end;
   end;
  if MaxSegmentSize <> 0 then
   begin
    if Winsock2.setsockopt(Handle,IPPROTO_TCP,TCP_MAXSEG,PChar(@MaxSegmentSize),SizeOf(MaxSegmentSize)) = SOCKET_ERROR then
     begin
      FLastError:=Winsock2.WSAGetLastError;
      
      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
      {$ENDIF}
      
      Exit;
     end;
   end;
   
  {Bind Socket}
  if Bind = SOCKET_ERROR then Exit;
  
  {Connect Socket}
  SockAddrLength:=0;
  SockAddr:=AllocateRemoteAddress(SockAddrLength);
  if SockAddr = nil then Exit;
  try
   FLastError:=ERROR_SUCCESS;
   if Winsock2.connect(Handle,SockAddr^,SockAddrLength) = SOCKET_ERROR then
    begin
     FLastError:=Winsock2.WSAGetLastError;
     
     {$IFDEF WINSOCK2_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  connect returned: = ' + Winsock2ErrorToString(FLastError));
     {$ENDIF}
     
     Exit;
    end;
   Result:=True;
  finally
   ReleaseAddress(SockAddr,SockAddrLength);
  end;
 finally
  if not(Result) then Disconnect;
 end;
end;

{==============================================================================}

function TWinsock2TCPClient.ConnectEx:Boolean;
{Similar to Connect except will get all addresses for RemoteHost and try each one in turn}
var
 Count:Integer;
 WorkHost:String;
 WorkBound:String;
 WorkRemote:String;
 RemoteAddresses:TStrings;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client: ConnectEx');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  RemoteHost = ' + FRemoteHost);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client:  RemoteAddress = ' + FRemoteAddress);
 {$ENDIF}
 
 if Length(FRemoteHost) = 0 then
  begin
   Result:=Connect;
  end
 else
  begin
   FLastError:=WSAEINVAL;
   WorkHost:=FRemoteHost;
   try
    FRemoteHost:='';
    RemoteAddresses:=ResolveHostEx(WorkHost,AF_UNSPEC,True);
    if RemoteAddresses = nil then Exit;
    try
     for Count:=0 to RemoteAddresses.Count - 1 do
      begin
       if Length(RemoteAddresses.Strings[Count]) <> 0 then
        begin
         WorkBound:=FBoundAddress;
         try
          WorkRemote:=FRemoteAddress;
          try
           FRemoteAddress:=RemoteAddresses.Strings[Count];
           Result:=Connect;
           if Result then Exit;
          finally
           if not(Result) then FRemoteAddress:=WorkRemote;
          end;
         finally
          if not(Result) then FBoundAddress:=WorkBound;
         end;
        end;
      end;
    finally
     RemoteAddresses.Free;
    end;
   finally
    FRemoteHost:=WorkHost;
   end;
  end;
end;

{==============================================================================}

function TWinsock2TCPClient.Disconnect:Boolean;
begin
 {}
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Client: Disconnect');
 {$ENDIF}

 if Length(FRemoteHost) <> 0 then FRemoteAddress:='';
 Result:=inherited Disconnect;
end;

{==============================================================================}
{==============================================================================}
{TWinsock2UDPClient}
constructor TWinsock2UDPClient.Create;
begin
 {}
 inherited Create;
 FUseConnect:=True;
 
 FRemotePort:=0;
 FRemoteHost:='';
 FRemoteAddress:='';
end;

{==============================================================================}

destructor TWinsock2UDPClient.Destroy;
begin
 {}
 if Connected then Disconnect;
 inherited Destroy;
end;

{==============================================================================}

procedure TWinsock2UDPClient.SetUseConnect(AUseConnect:Boolean);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client: SetUseConnect');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 FUseConnect:=AUseConnect;
end;

{==============================================================================}

procedure TWinsock2UDPClient.SetFamily(AFamily:Integer);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client: SetFamily');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=WSAEINVAL;
 case AFamily of
  AF_UNSPEC,AF_INET,AF_INET6:begin
    {Unspecified, IPv4 or IPv6}
    FLastError:=ERROR_SUCCESS;
    if FFamily <> AFamily then FBoundAddress:='';
    if FFamily <> AFamily then FRemoteAddress:='';
    FFamily:=AFamily;
   end;
 end;
end;

{==============================================================================}

procedure TWinsock2UDPClient.SetBoundAddress(const ABoundAddress:String);
var
 WorkInt:Integer;
 WorkBuffer:String;
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client: SetBoundAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  BoundAddress = ' + ABoundAddress);
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 {Set Bound Address}
 FLastError:=ERROR_SUCCESS;
 WorkBuffer:=RemoveBraces(ABoundAddress);
 if FBoundAddress = WorkBuffer then Exit;
 FBoundAddress:=WorkBuffer;
 {Check Bound Address}
 if Length(FBoundAddress) = 0 then
  begin
   {Check Remote Address}
   if Length(FRemoteAddress) = 0 then
    begin
     FFamily:=AF_UNSPEC;
    end;
  end
 else
  begin
   {Check Remote Address}
   if Length(FRemoteAddress) = 0 then
    begin
     FFamily:=ResolveFamily(FBoundAddress);
    end
   else
    begin
     {Set Family}
     FFamily:=ResolveFamily(FRemoteAddress);
     {Compare Family}
     WorkInt:=ResolveFamily(FBoundAddress);
     if WorkInt <> FFamily then
      begin
       FBoundAddress:='';
      end;
    end;
  end;
end;

{==============================================================================}

procedure TWinsock2UDPClient.SetRemotePort(ARemotePort:Word);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client: SetRemotePort');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  RemotePort = ' + IntToStr(ARemotePort));
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=ERROR_SUCCESS;
 FRemotePort:=ARemotePort;
end;

{==============================================================================}

procedure TWinsock2UDPClient.SetRemoteHost(const ARemoteHost:String);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client: SetRemoteHost');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  RemoteHost = ' + ARemoteHost);
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 {Set Remote Host}
 FLastError:=ERROR_SUCCESS;
 FRemoteHost:=ARemoteHost;
 {Check Remote Host}
 if Length(FRemoteHost) <> 0 then
  begin
   {Set Remote Address}
   FRemoteAddress:='';
   {Check Bound Address}
   if Length(FBoundAddress) = 0 then
    begin
     FFamily:=AF_UNSPEC;
    end
   else
    begin
     FFamily:=ResolveFamily(FBoundAddress);
    end;
  end
end;

{==============================================================================}

procedure TWinsock2UDPClient.SetRemoteAddress(const ARemoteAddress:String);
var
 WorkInt:Integer;
 WorkBuffer:String;
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client: SetRemoteAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  RemoteAddress = ' + ARemoteAddress);
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 {Set Remote Address}
 FLastError:=ERROR_SUCCESS;
 WorkBuffer:=RemoveBraces(ARemoteAddress);
 if FRemoteAddress = WorkBuffer then Exit;
 FRemoteAddress:=WorkBuffer;
 {Check Remote Address}
 if Length(FRemoteAddress) = 0 then
  begin
   {Check Bound Address}
   if Length(FBoundAddress) = 0 then
    begin
     FFamily:=AF_UNSPEC;
    end;
  end
 else
  begin
   {Set Remote Host}
   FRemoteHost:='';
   {Check Bound Address}
   if Length(FBoundAddress) = 0 then
    begin
     FFamily:=ResolveFamily(FRemoteAddress);
    end
   else
    begin
     {Set Family}
     FFamily:=ResolveFamily(FRemoteAddress);
     {Compare Family}
     WorkInt:=ResolveFamily(FBoundAddress);
     if WorkInt <> FFamily then
      begin
       FBoundAddress:='';
      end;
    end;
  end;
end;

{==============================================================================}

function TWinsock2UDPClient.AllocateFamily:Boolean;
{Check Family, Bound Address and Remote Address, normalise as needed}
var
 WorkInt:Integer;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client: AllocateFamily');
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 FLastError:=WSAEINVAL;
 {Check Remote Address}
 if Length(FRemoteAddress) = 0 then
  begin
   Result:=inherited AllocateFamily;
  end
 else 
  begin
   {Check Family}
   case FFamily of
    AF_UNSPEC:begin
      {Unspecified}
      FFamily:=ResolveFamily(FRemoteAddress);
     end;
    AF_INET,AF_INET6:begin
      {IPv4 or IPv6}
      WorkInt:=ResolveFamily(FRemoteAddress);
      if WorkInt <> FFamily then
       begin
        FFamily:=WorkInt;
       end;
     end;
   end;
   {Check Bound Address}
   if Length(FBoundAddress) = 0 then
    begin
     {Check Family}
     case FFamily of
      AF_INET:begin
        {IPv4}
        FBoundAddress:=INET_ADDRSTR_ANY;
        FLastError:=ERROR_SUCCESS;
        Result:=True;
       end;
      AF_INET6:begin
        {IPv6}
        FBoundAddress:=INET6_ADDRSTR_INIT;
        FLastError:=ERROR_SUCCESS;
        Result:=True;
       end;
     end;
    end
   else
    begin
     {Check Family}
     case FFamily of
      AF_INET:begin
        {IPv4}
        WorkInt:=ResolveFamily(FBoundAddress);
        if WorkInt <> FFamily then
         begin
          FBoundAddress:=INET_ADDRSTR_ANY;
         end;
        FLastError:=ERROR_SUCCESS;
        Result:=True;
       end;
      AF_INET6:begin
        {IPv6}
        WorkInt:=ResolveFamily(FBoundAddress);
        if WorkInt <> FFamily then
         begin
          FBoundAddress:=INET6_ADDRSTR_INIT;
         end;
        FLastError:=ERROR_SUCCESS;
        Result:=True;
       end;
     end;
    end;
  end;
  
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  RemoteAddress = ' + FRemoteAddress);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  BoundAddress = ' + FBoundAddress);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  Family = ' + AddressFamilyToString(FFamily));
 {$ENDIF}
end;

{==============================================================================}

function TWinsock2UDPClient.AllocateRemoteAddress(var ALength:Integer):PSockAddr;
{Allocate a socket address structure for the remote address and port}
{Note: Change this to use InetPton for both IPv4 and IPv6 addresses}
var
 InAddr:TInAddr;
 SockAddr:TSockAddrIn6;
 {BufferLength:LongInt;}
begin
 {}
 Result:=nil;
 ALength:=0;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client: AllocateRemoteAddress');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  RemoteAddress = ' + FRemoteAddress);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  RemotePort = ' + IntToStr(FRemotePort));
 {$ENDIF}
 
 FLastError:=WSAEINVAL;
 if Length(FRemoteAddress) = 0 then Exit;

 {Allocate Address}
 Result:=AllocateAddress(ALength);
 if Result = nil then Exit;

 {Check Family}
 case Result.sin_family of
  AF_INET:begin
    {IPv4}
    {Setup remote address}
    Result.sin_port:=Winsock2.htons(FRemotePort);
    InAddr.S_addr:=Winsock2.inet_addr(PChar(FRemoteAddress));
    if (InAddr.S_addr = INADDR_NONE) and (FRemoteAddress <> INET_ADDRSTR_BROADCAST) then
     begin
      FLastError:=WSAEINVAL;
      ReleaseAddress(Result,ALength);
      
      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  inet_addr returned: = INADDR_NONE');
      {$ENDIF}
      
      Exit;
     end;
    if InAddr.S_addr = INADDR_ANY then
     begin
      FLastError:=WSAEINVAL;
      ReleaseAddress(Result,ALength);
      
      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  inet_addr returned: = INADDR_ANY');
      {$ENDIF}
      
      Exit;
     end;
    Result.sin_addr.S_addr:=InAddr.S_addr;
    FLastError:=ERROR_SUCCESS;
   end;
  AF_INET6:begin
    {IPv6}
    {Setup remote address}
    PSockAddrIn6(Result).sin6_port:=Winsock2.htons(FRemotePort);
    
    if Winsock2.InetPtonA(AF_INET6,PChar(FRemoteAddress),@SockAddr.sin6_addr) = SOCKET_ERROR then
     begin
      FLastError:=Winsock2.WSAGetLastError;
      ReleaseAddress(Result,ALength);
      
      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  InetPtonA returned: ' + Winsock2ErrorToString(FLastError));
      {$ENDIF}
      
      Exit;
     end;
    
    {BufferLength:=SizeOf(TSockAddrIn6);
    if Winsock2.WSAStringToAddressA(PChar(FRemoteAddress),FFamily,nil,PSockAddrIn(@SockAddr)^,BufferLength) = SOCKET_ERROR then
     begin
      FLastError:=Winsock2.WSAGetLastError;
      ReleaseAddress(Result,ALength);
      Exit;
     end;}
     
    PSockAddrIn6(Result).sin6_addr.s6_addr:=SockAddr.sin6_addr.s6_addr;
    FLastError:=ERROR_SUCCESS;
   end;
 end;
end;

{==============================================================================}

function TWinsock2UDPClient.Connect:Boolean;
var
 WorkInt:Integer;
 WorkBool:LongBool;
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
begin
 {}
 Result:=False;
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client: Connect');
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  RemoteHost = ' + FRemoteHost);
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  RemoteAddress = ' + FRemoteAddress);
 {$ENDIF}
 
 FLastError:=WSAEISCONN;
 if Connected then Exit;

 {Check Remote Host}
 if Length(FRemoteHost) <> 0 then
  begin
   WorkInt:=FFamily;
   try
    if FFamily = AF_UNSPEC then FFamily:=AF_INET;
    FRemoteAddress:=ResolveHost(FRemoteHost);
    if Length(FRemoteAddress) = 0 then
     begin
      if FFamily = AF_INET then FFamily:=AF_INET6 else FFamily:=AF_INET;
      FRemoteAddress:=ResolveHost(FRemoteHost);
     end;

    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  ResolvedAddress = ' + FRemoteAddress);
    {$ENDIF}
   finally
    FFamily:=WorkInt;
   end;
  end;
  
 {Check Family}
 if not AllocateFamily then Exit;
 
 {Create Socket}
 if AllocateSocket(FSocketType) = INVALID_SOCKET then Exit;
 try
  {Set Options}
  if BroadcastEnabled then
   begin
    WorkBool:=BroadcastEnabled;
    if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_BROADCAST,PChar(@WorkBool),SizeOf(WorkBool)) = SOCKET_ERROR then
     begin
      FLastError:=Winsock2.WSAGetLastError;
      
      {$IFDEF WINSOCK2_DEBUG}
      if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
      {$ENDIF}
      
      Exit;
     end;
   end;
  
  {Bind Socket}
  if Bind = SOCKET_ERROR then Exit;
  
  {Connect Socket}
  if UseConnect then
   begin
    SockAddrLength:=0;
    SockAddr:=AllocateRemoteAddress(SockAddrLength);
    if SockAddr = nil then Exit;
    try
     FLastError:=ERROR_SUCCESS;
     if Winsock2.connect(Handle,SockAddr^,SockAddrLength) = SOCKET_ERROR then
      begin
       FLastError:=Winsock2.WSAGetLastError;
       
       {$IFDEF WINSOCK2_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client:  connect returned: = ' + Winsock2ErrorToString(FLastError));
       {$ENDIF}
       
       Exit;
      end;
     Result:=True;
    finally
     ReleaseAddress(SockAddr,SockAddrLength);
    end;
   end
  else
   begin
    FLastError:=ERROR_SUCCESS;
    Result:=True;
   end;   
 finally
  if not(Result) then Disconnect;
 end;
end;

{==============================================================================}

function TWinsock2UDPClient.Disconnect:Boolean;
begin
 {}
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Client: Disconnect');
 {$ENDIF}

 if Length(FRemoteHost) <> 0 then FRemoteAddress:='';
 Result:=inherited Disconnect;
end;

{==============================================================================}
{==============================================================================}
{TWinsock2TCPServer}
constructor TWinsock2TCPServer.Create(AListener:TWinsock2TCPListener);
begin
 {}
 inherited Create;
 FListener:=AListener;
end;

{==============================================================================}

procedure TWinsock2TCPServer.SetHandle(AHandle:TSocket);
begin
 {}
 FHandle:=AHandle;
end;

{==============================================================================}

procedure TWinsock2TCPServer.SetLastError(ALastError:LongInt);
begin
 {}
 FLastError:=ALastError;
end;

{==============================================================================}

function TWinsock2TCPServer.Disconnect:Boolean;
begin
 {}
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Server: Disconnect');
 {$ENDIF}

 FPeerPort:=0;
 FPeerAddress:='';
 Result:=inherited Disconnect;
end;

{==============================================================================}
{==============================================================================}
{TWinsock2TCPServerThread}
constructor TWinsock2TCPServerThread.Create(AServer:TWinsock2TCPServer);
begin
 {}
 inherited Create(True,THREAD_STACK_DEFAULT_SIZE);
 FData:=nil;
 FServer:=AServer;
 FreeOnTerminate:=False;
end;

{==============================================================================}

destructor TWinsock2TCPServerThread.Destroy;
begin
 {}
 if FData <> nil then FData.Free;
 if FServer <> nil then FServer.Free;
 inherited Destroy;
end;

{==============================================================================}

procedure TWinsock2TCPServerThread.AfterExecution;
begin
 {}
 if FServer = nil then Exit;
 if FServer.Listener = nil then Exit;
 FServer.Listener.DoDisconnect(Self);
 FServer.Listener.Threads.Terminate(Self);
end;

{==============================================================================}

procedure TWinsock2TCPServerThread.BeforeExecution;
begin
 {}
 if FServer = nil then Exit;
 if FServer.Listener = nil then Exit;
 FServer.Listener.DoConnect(Self);

 {Set Name} 
 ThreadSetName(GetCurrentThreadID,WINSOCK_TCP_SERVER_THREAD_NAME);
 
 {Set Priority}
 ThreadSetPriority(GetCurrentThreadID,WINSOCK_TCP_SERVER_THREAD_PRIORITY);
end;

{==============================================================================}

procedure TWinsock2TCPServerThread.Execution;
var
 Success:Boolean;
begin
 {}
 Success:=False;
 try
  if FServer = nil then Exit;
  if FServer.Listener = nil then Exit;
  
  if not FServer.Listener.DoExecute(Self) then Exit;
  Success:=True;
 finally
  if not Success then Terminate;
 end;
end;

{==============================================================================}
{==============================================================================}
{TWinsock2TCPListenerThread}
constructor TWinsock2TCPListenerThread.Create(AListener:TWinsock2TCPListener);
begin
 {}
 inherited Create(True,THREAD_STACK_DEFAULT_SIZE);
 FListener:=AListener;
 FreeOnTerminate:=True;
end;

{==============================================================================}

procedure TWinsock2TCPListenerThread.AfterExecution;
begin
 {}
 if FListener = nil then Exit;
 FListener.Disconnect;
end;

{==============================================================================}

procedure TWinsock2TCPListenerThread.BeforeExecution;
begin
 {}
 {Set Name}
 ThreadSetName(GetCurrentThreadID,WINSOCK_TCP_LISTENER_THREAD_NAME);
 
 {Set Priority}
 ThreadSetPriority(GetCurrentThreadID,WINSOCK_TCP_LISTENER_THREAD_PRIORITY);
end;

{==============================================================================}

procedure TWinsock2TCPListenerThread.Execution;
var
 Success:Boolean;
 NewHandle:TSocket;
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
 SockName:PSockAddr;
 SockNameLength:Integer;
 Server:TWinsock2TCPServer;
 Thread:TWinsock2TCPServerThread;
begin
 {}
 Success:=False;
 try
  if FListener = nil then Exit;

  {Allocate Address}
  SockAddrLength:=0;
  SockAddr:=FListener.AllocateAddress(SockAddrLength);
  if SockAddr = nil then Exit;
  try
   {Accept}
   FListener.SetLastError(ERROR_SUCCESS);
   NewHandle:=Winsock2.accept(FListener.Handle,SockAddr,SockAddrLength);
   if NewHandle = INVALID_SOCKET then
    begin
     FListener.SetLastError(Winsock2.WSAGetLastError);
    end
   else
    begin
     {Allocate Address}
     SockNameLength:=0;
     SockName:=FListener.AllocateAddress(SockNameLength);
     if SockName = nil then Exit;
     try
      {Get Local Address and Port}
      if Winsock2.getsockname(NewHandle,SockName^,SockNameLength) = SOCKET_ERROR then
       begin
        FListener.SetLastError(Winsock2.WSAGetLastError);
       end
      else
       begin
        {Create Server}
        Server:=TWinsock2TCPServer.Create(FListener);
        Server.BoundAddress:=FListener.SockAddrToAddress(SockName,SockNameLength);
        Server.BoundPort:=FListener.SockAddrToPort(SockName,SockNameLength);
        Server.FPeerAddress:=FListener.SockAddrToAddress(SockAddr,SockAddrLength);
        Server.FPeerPort:=FListener.SockAddrToPort(SockAddr,SockAddrLength);
        Server.SetHandle(NewHandle);
        
        {Create Thread}
        Thread:=nil;
        if Assigned(FListener.OnCreateThread) then
         begin
          FListener.OnCreateThread(Server,Thread);
         end;
        if Thread = nil then
         begin
          Thread:=TWinsock2TCPServerThread.Create(Server);
         end;
        FListener.Threads.Add(Thread);
        
        {Start Thread}
        Thread.Start;
        
        Success:=True;
       end;
     finally
      FListener.ReleaseAddress(SockName,SockNameLength);
     end;
    end;
  finally
   FListener.ReleaseAddress(SockAddr,SockAddrLength);
  end;
 finally
  if not Success then Terminate;
 end;
end;

{==============================================================================}
{==============================================================================}
{TWinsock2TCPServerThreads}
procedure TWinsock2TCPServerThreads.TerminateThread(AThread:TWinsock2TCPServerThread);
begin
 {}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 if GetCurrentThreadID <> AThread.ThreadID then
  begin
   AThread.FreeOnTerminate:=False;
   AThread.Server.Disconnect;
   AThread.TerminateAndWaitFor;
   AThread.Free;
  end
 else
  begin
   AThread.FreeOnTerminate:=True;
   AThread.Server.Disconnect;
  end;
end;

{==============================================================================}

procedure TWinsock2TCPServerThreads.TerminateAll;
var
 Thread:TWinsock2TCPServerThread;
begin
 {}
 Thread:=TWinsock2TCPServerThread(First);
 while Thread <> nil do
  begin
   Remove(Thread);
   TerminateThread(Thread);
   
   Thread:=TWinsock2TCPServerThread(First);
  end;
end;

{==============================================================================}

function TWinsock2TCPServerThreads.Terminate(AThread:TWinsock2TCPServerThread):Boolean;
begin
 {}
 Result:=False;
 
 if AThread = nil then Exit;
 
 Result:=Remove(AThread);
 if Result then TerminateThread(AThread);
end;

{==============================================================================}
{==============================================================================}
{TWinsock2TCPListener}
constructor TWinsock2TCPListener.Create;
begin
 {}
 inherited Create;
 FThreads:=TWinsock2TCPServerThreads.Create;
end;

{==============================================================================}

destructor TWinsock2TCPListener.Destroy;
begin
 {}
 Active:=False;
 FThreads.TerminateAll;
 FThreads.Free;
 inherited Destroy;
end;

{==============================================================================}

procedure TWinsock2TCPListener.SetActive(AActive:Boolean);
begin
 {}
 if FActive = AActive then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Listener: SetActive');
 {$ENDIF}
 
 if AActive then
  begin
   {Allocate Family}
   if not AllocateFamily then Exit;
   
   {Allocate Socket}
   if AllocateSocket(FSocketType) = INVALID_SOCKET then Exit;
   
   {Bind} 
   if Bind = SOCKET_ERROR then Exit;
   
   {Listen}
   if Listen(FBacklog) = SOCKET_ERROR then Exit;
   
   {Create Thread}
   FListenerThread:=TWinsock2TCPListenerThread.Create(Self);
   
   {Start Thread}
   FListenerThread.Start;
  end
 else
  begin
   {Disconnect Thread}
   FListenerThread.FListener.Disconnect;
   
   {Terminate Thread}
   FListenerThread.TerminateAndWaitFor;
  end;
  
 FActive:=AActive;
end;

{==============================================================================}

procedure TWinsock2TCPListener.SetLastError(ALastError:LongInt);
begin
 {}
 FLastError:=ALastError;
end;

{==============================================================================}

procedure TWinsock2TCPListener.DoConnect(AThread:TWinsock2TCPServerThread);
var
 WorkBool:LongBool;
begin
 {}
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Listener: DoConnect');
 {$ENDIF}

 {Set Options}
 if not UseNagle then
  begin
   WorkBool:=not UseNagle;
   if Winsock2.setsockopt(AThread.Server.Handle,IPPROTO_TCP,TCP_NODELAY,PChar(@WorkBool),SizeOf(WorkBool)) = SOCKET_ERROR then
    begin
     FLastError:=Winsock2.WSAGetLastError;
      
     {$IFDEF WINSOCK2_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Listener:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
     {$ENDIF}
      
     Exit;
    end;
  end;
 if UseKeepalive then
  begin
   WorkBool:=UseKeepalive;
   if Winsock2.setsockopt(AThread.Server.Handle,SOL_SOCKET,SO_KEEPALIVE,PChar(@WorkBool),SizeOf(WorkBool)) = SOCKET_ERROR then
    begin
     FLastError:=Winsock2.WSAGetLastError;
      
     {$IFDEF WINSOCK2_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Listener:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
     {$ENDIF}
      
     Exit;
    end;
  end;
 if MaxSegmentSize <> 0 then
  begin
   if Winsock2.setsockopt(AThread.Server.Handle,IPPROTO_TCP,TCP_MAXSEG,PChar(@MaxSegmentSize),SizeOf(MaxSegmentSize)) = SOCKET_ERROR then
    begin
     FLastError:=Winsock2.WSAGetLastError;
      
     {$IFDEF WINSOCK2_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Listener:  setsockopt returned: = ' + Winsock2ErrorToString(FLastError));
     {$ENDIF}
      
     Exit;
    end;
  end;
 
 if Assigned(FOnConnect) then FOnConnect(AThread);
end;

{==============================================================================}

procedure TWinsock2TCPListener.DoDisconnect(AThread:TWinsock2TCPServerThread);
begin
 {}
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Listener: DoDisconnect');
 {$ENDIF}

 if Assigned(FOnDisconnect) then FOnDisconnect(AThread);
end;

{==============================================================================}

function TWinsock2TCPListener.DoExecute(AThread:TWinsock2TCPServerThread):Boolean;
begin
 {}
 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 TCP Listener: DoExecute');
 {$ENDIF}

 Result:=True;
 if Assigned(FOnExecute) then Result:=FOnExecute(AThread);
end;

{==============================================================================}
{==============================================================================}
{TWinsock2UDPServer}
constructor TWinsock2UDPServer.Create(AListener:TWinsock2UDPListener);
begin
 {}
 inherited Create;
 FPeerPort:=0;
 FPeerAddress:='';
 
 FUseListener:=False;
 FListener:=AListener;

 FBuffer:=nil;
 
 if FListener <> nil then
  begin
   ReuseAddress:=AListener.ReuseAddress;
   Family:=AListener.Family;
   SocketType:=AListener.SocketType;
   Protocol:=AListener.Protocol;
   BufferSize:=AListener.BufferSize;
   BroadcastEnabled:=AListener.BroadcastEnabled;

   FUseListener:=AListener.UseListener;
  end;
end;

{==============================================================================}

destructor TWinsock2UDPServer.Destroy; 
begin
 {}
 FBuffer:=nil;
 FListener:=nil;
 inherited Destroy;
end;

{==============================================================================}

procedure TWinsock2UDPServer.SetUseListener(AUseListener:Boolean);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 {$IFDEF WINSOCK2_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2 UDP Server: SetUseListener');
 {$ENDIF}
 
 FLastError:=WSAEINVAL;
 if FListener = nil then Exit; 
 
 FUseListener:=AUseListener;
end;

{==============================================================================}

function TWinsock2UDPServer.GetData:Pointer; 
begin
 {}
 Result:=nil;
 
 if FBuffer = nil then Exit;
 
 Result:=FBuffer.Data;
end;

{==============================================================================}

function TWinsock2UDPServer.GetSize:Integer; 
begin
 {}
 Result:=0;
 
 if FBuffer = nil then Exit;
 
 Result:=FBuffer.Size;
end;
  
{==============================================================================}
  
function TWinsock2UDPServer.GetCount:Integer;   
begin
 {}
 Result:=0;
 
 if FBuffer = nil then Exit;
 
 Result:=FBuffer.Count;
end;

{==============================================================================}

procedure TWinsock2UDPServer.SetBuffer(ABuffer:TWinsock2UDPServerBuffer); 
begin
 {}
 FBuffer:=ABuffer;
end;

{==============================================================================}

procedure TWinsock2UDPServer.SetLastError(ALastError:LongInt); 
begin
 {}
 FLastError:=ALastError;
end;

{==============================================================================}

function TWinsock2UDPServer.RecvData(AData:Pointer;ACount:Integer):Integer; 
begin
 {}
 if FUseListener then
  begin
   Result:=0;
   FLastError:=WSAEINVAL;
   
   {Invalid, the listener thread will receive all data from the listening socket}
  end
 else
  begin
   Result:=inherited RecvData(AData,ACount);
  end;  
end;

{==============================================================================}

function TWinsock2UDPServer.SendData(AData:Pointer;ACount:Integer):Integer; 
begin
 {}
 if FUseListener then
  begin
   Result:=0;
   FLastError:=WSAEINVAL;
   if FListener = nil then Exit; 
  
   Result:=FListener.SendData(AData,ACount);
  end
 else
  begin
   Result:=inherited SendData(AData,ACount);
  end;  
end;

{==============================================================================}

function TWinsock2UDPServer.BroadcastData(APort:Word;AData:Pointer;ACount:Integer):Integer; 
begin
 {}
 if FUseListener then
  begin
   Result:=0;
   FLastError:=WSAEINVAL;
   if FListener = nil then Exit; 
  
   Result:=FListener.BroadcastData(APort,AData,ACount);
  end
 else
  begin
   Result:=inherited BroadcastData(APort,AData,ACount);
  end;  
end;

{==============================================================================}

function TWinsock2UDPServer.RecvDataFrom(var AHost:String;var APort:Word;AData:Pointer;ACount:Integer):Integer; 
begin
 {}
 if FUseListener then
  begin
   Result:=0;
   FLastError:=WSAEINVAL;
   
   {Invalid, the listener thread will receive all data from the listening socket}
  end
 else
  begin
   Result:=inherited RecvDataFrom(AHost,APort,AData,ACount);
  end;  
end;

{==============================================================================}

function TWinsock2UDPServer.SendDataTo(const AHost:String;APort:Word;AData:Pointer;ACount:Integer):Integer; 
begin
 {}
 if FUseListener then
  begin
   Result:=0;
   FLastError:=WSAEINVAL;
   if FListener = nil then Exit; 
  
   Result:=FListener.SendDataTo(AHost,APort,AData,ACount);
  end
 else
  begin
   Result:=inherited SendDataTo(AHost,APort,AData,ACount);
  end;  
end;

{==============================================================================}

function TWinsock2UDPServer.BroadcastDataTo(const AAddress:String;APort:Word;AData:Pointer;ACount:Integer):Integer; 
begin
 {}
 if FUseListener then
  begin
   Result:=0;
   FLastError:=WSAEINVAL;
   if FListener = nil then Exit; 
   
   Result:=FListener.BroadcastDataTo(AAddress,APort,AData,ACount);
  end
 else
  begin
   Result:=inherited BroadcastDataTo(AAddress,APort,AData,ACount);
  end;  
end;

{==============================================================================}
{==============================================================================}
{TWinsock2UDPServerThread}
constructor TWinsock2UDPServerThread.Create(AServer:TWinsock2UDPServer);
begin
 {}
 inherited Create(True,THREAD_STACK_DEFAULT_SIZE);
 FLock:=CriticalSectionCreate;
 
 FActive:=False;
 FData:=nil;
 FServer:=AServer;
 FreeOnTerminate:=False;
end;

{==============================================================================}

destructor TWinsock2UDPServerThread.Destroy; 
begin
 {}
 AcquireLock;
 try
  if FData <> nil then FData.Free;
  if FServer <> nil then FServer.Free;
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TWinsock2UDPServerThread.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TWinsock2UDPServerThread.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TWinsock2UDPServerThread.GetActive:Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;

 Result:=FActive;

 ReleaseLock;
end;

{==============================================================================}

procedure TWinsock2UDPServerThread.SetActive(AActive:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 FActive:=AActive;

 ReleaseLock;
end;

{==============================================================================}

procedure TWinsock2UDPServerThread.AfterExecution;
begin
 {}
 if FServer = nil then Exit;
 if FServer.Listener = nil then Exit;
 FServer.Listener.Threads.Terminate(Self);
end;

{==============================================================================}

procedure TWinsock2UDPServerThread.BeforeExecution;
begin
 {}
 {Set Name}
 ThreadSetName(GetCurrentThreadID,WINSOCK_UDP_SERVER_THREAD_NAME);
 
 {Set Priority}
 ThreadSetPriority(GetCurrentThreadID,WINSOCK_UDP_SERVER_THREAD_PRIORITY);
end;

{==============================================================================}

procedure TWinsock2UDPServerThread.Execution; 
var
 Success:Boolean;
begin
 {}
 if Active then
  begin
   Success:=False;
   try
    if FServer = nil then Exit;
    if FServer.Buffer = nil then Exit;
    if FServer.Listener = nil then Exit;
    
    if not FServer.Listener.DoExecute(Self) then Exit;
    Success:=True;
   finally
    FServer.Listener.Buffers.ReleaseBuffer(Server.Buffer);
    if not Success then Terminate else FServer.Listener.Threads.ReleaseThread(Self);
   end;
  end
 else
  begin
   Suspended:=True; {Suspend;} {Suspend is deprecated}
  end;
end;

{==============================================================================}
{==============================================================================}
{TWinsock2UDPListenerThread}
constructor TWinsock2UDPListenerThread.Create(AListener:TWinsock2UDPListener);
begin
 {}
 inherited Create(True,THREAD_STACK_DEFAULT_SIZE);
 FListener:=AListener;
 FreeOnTerminate:=True;
end;

{==============================================================================}

destructor TWinsock2UDPListenerThread.Destroy; 
begin
 {}
 FListener:=nil;
 inherited Destroy;
end;

{==============================================================================}

procedure TWinsock2UDPListenerThread.AfterExecution; 
begin
 {}
 if FListener = nil then Exit;
 FListener.Disconnect;
end;

{==============================================================================}

procedure TWinsock2UDPListenerThread.BeforeExecution;
begin
 {}
 {Set Name}
 ThreadSetName(GetCurrentThreadID,WINSOCK_UDP_LISTENER_THREAD_NAME);
 
 {Set Priority}
 ThreadSetPriority(GetCurrentThreadID,WINSOCK_UDP_LISTENER_THREAD_PRIORITY);
end;

{==============================================================================}

procedure TWinsock2UDPListenerThread.Execution; 
var
 Success:Boolean;
 SockAddr:PSockAddr;
 SockAddrLength:Integer;
 SockName:PSockAddr;
 SockNameLength:Integer;
 Thread:TWinsock2UDPServerThread;
 Buffer:TWinsock2UDPServerBuffer;
begin
 {}
 Success:=False;
 try
  if FListener = nil then Exit;
 
  {Get Buffer}
  Buffer:=FListener.Buffers.GetBuffer;
  if Buffer = nil then Exit;
  
  {Set Active}
  Buffer.Active:=True;
  
  {Allocate Address}
  SockAddrLength:=0;
  SockAddr:=FListener.AllocateAddress(SockAddrLength);
  if SockAddr = nil then Exit;
  try
   {RecvFrom}
   FListener.SetLastError(ERROR_SUCCESS);
   Buffer.Count:=Winsock2.recvfrom(FListener.Handle,Buffer.Data^,Buffer.Size,0,SockAddr^,SockAddrLength); 
   if Buffer.Count = SOCKET_ERROR then
    begin
     FListener.SetLastError(Winsock2.WSAGetLastError);
    end
   else
    begin
     if Buffer.Count = 0 then
      begin
       FListener.SetLastError(ERROR_SUCCESS);
      end
     else
      begin
       {Allocate Address}
       SockNameLength:=0;
       SockName:=FListener.AllocateAddress(SockNameLength);
       if SockName = nil then Exit;
       try
        {Get Local Address and Port}
        if Winsock2.getsockname(FListener.Handle,SockName^,SockNameLength) = SOCKET_ERROR then
         begin
          FListener.SetLastError(Winsock2.WSAGetLastError);
         end
        else
         begin
          {Get Thread}
          Thread:=FListener.Threads.GetThread;
          if Thread <> nil then
           begin
            {Update Server}
            Thread.Server.BoundAddress:=FListener.SockAddrToAddress(SockName,SockNameLength);
            Thread.Server.BoundPort:=FListener.SockAddrToPort(SockName,SockNameLength);
            Thread.Server.FPeerAddress:=FListener.SockAddrToAddress(SockAddr,SockAddrLength);
            Thread.Server.FPeerPort:=FListener.SockAddrToPort(SockAddr,SockAddrLength);
            Thread.Server.SetBuffer(Buffer);
          
            {Start Thread}
            Thread.Active:=True;
            Thread.Start;
            
            Success:=True;
           end; 
         end;
       finally
        FListener.ReleaseAddress(SockName,SockNameLength);
       end;
      end;
    end;    
  finally
   FListener.ReleaseAddress(SockAddr,SockAddrLength);
  end;
 finally
  if not Success then Terminate;
 end;
end;
   
{==============================================================================}
{==============================================================================}
{TWinsock2UDPServerThreads}
constructor TWinsock2UDPServerThreads.Create(AListener:TWinsock2UDPListener);
begin
 {}
 inherited Create;
 FMin:=5;
 FMax:=10;
 
 FListener:=AListener;
end;
   
{==============================================================================}

function TWinsock2UDPServerThreads.GetMin:Integer;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FMin;

 ReleaseLock;
end;

{==============================================================================}

procedure TWinsock2UDPServerThreads.SetMin(AMin:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FMin:=AMin;
 if FMin = 0 then FMin:=1;
 if FMin > FMax then FMax:=FMin;
 
 ReleaseLock;
end;
   
{==============================================================================}

function TWinsock2UDPServerThreads.GetMax:Integer;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FMax;

 ReleaseLock;
end;

{==============================================================================}

procedure TWinsock2UDPServerThreads.SetMax(AMax:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FMax:=AMax;
 if FMax < FMin then FMax:=FMin;

 ReleaseLock;
end;
   
{==============================================================================}

procedure TWinsock2UDPServerThreads.CreateThreads;
begin
 {}
 while Count < FMin do
  begin
   if CreateThread(False) = nil then Exit;
  end;
end;
   
{==============================================================================}

function TWinsock2UDPServerThreads.CreateThread(AForce:Boolean):TWinsock2UDPServerThread;
var
 WorkBool:LongBool;
 Server:TWinsock2UDPServer;
begin
 {}
 Result:=nil;
 if FListener = nil then Exit;
  
 if (Count < FMax) or AForce then
  begin
   {Create Server}
   Server:=TWinsock2UDPServer.Create(FListener);
   try
    {Setup Server}
    if not Server.UseListener then
     begin
      {Allocate Family}
      if not Server.AllocateFamily then Exit;
    
      {Allocate Socket}
      if Server.AllocateSocket(Server.SocketType) = INVALID_SOCKET then Exit;
    
      {Set Options}
      if Server.BroadcastEnabled then
       begin
        WorkBool:=Server.BroadcastEnabled;
        if Winsock2.setsockopt(Server.Handle,SOL_SOCKET,SO_BROADCAST,PChar(@WorkBool),SizeOf(WorkBool)) = SOCKET_ERROR then
         begin
          Server.SetLastError(Winsock2.WSAGetLastError);
          Exit;
         end;
       end;
    
      {Bind} 
      if Server.Bind = SOCKET_ERROR then Exit;
     end; 
     
    {Create Thread}
    if Assigned(FListener.OnCreateThread) then
     begin
      FListener.OnCreateThread(Server,Result);
     end;
    if Result = nil then
     begin
      Result:=TWinsock2UDPServerThread.Create(Server);
     end;
    Add(Result);
     
    {Start Thread}
    Result.Start;
   finally
    if Result = nil then Server.Free;
   end;    
  end;
end;

{==============================================================================}
   
procedure TWinsock2UDPServerThreads.TerminateThread(AThread:TWinsock2UDPServerThread);
begin
 {}
 if AThread = nil then Exit;
 if AThread.Server = nil then Exit;
 
 if GetCurrentThreadID <> AThread.ThreadID then
  begin
   AThread.FreeOnTerminate:=False;
   AThread.Server.Disconnect;
   AThread.TerminateAndWaitFor;
   AThread.Free;
  end
 else
  begin
   AThread.FreeOnTerminate:=True;
   AThread.Server.Disconnect;
   AThread.Terminate;
  end;
end;
   
{==============================================================================}
   
function TWinsock2UDPServerThreads.GetThread:TWinsock2UDPServerThread;
var
 Thread:TWinsock2UDPServerThread;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  Thread:=TWinsock2UDPServerThread(First);
  while Thread <> nil do
   begin
    if not Thread.Active then
     begin
      Result:=Thread;
      Exit;
     end;
     
    Thread:=TWinsock2UDPServerThread(Thread.Next);
   end;
 finally
  ReleaseLock;
 end;
 
 if Result = nil then Result:=CreateThread(True);
end;

{==============================================================================}

procedure TWinsock2UDPServerThreads.ReleaseThread(AThread:TWinsock2UDPServerThread);
begin
 {}
 if AThread = nil then Exit;
 
 AThread.Data:=nil;
 AThread.Active:=False;

 if Count > FMax then 
  begin
   Remove(AThread);
   TerminateThread(AThread);
  end;
end;
 
{==============================================================================}

procedure TWinsock2UDPServerThreads.TerminateAll;
var
 Thread:TWinsock2UDPServerThread;
begin
 {}
 Thread:=TWinsock2UDPServerThread(First);
 while Thread <> nil do
  begin
   Remove(Thread);
   TerminateThread(Thread);
   
   Thread:=TWinsock2UDPServerThread(First);
  end;
end;

{==============================================================================}

function TWinsock2UDPServerThreads.Terminate(AThread:TWinsock2UDPServerThread):Boolean;
begin
 {}
 Result:=False;
 
 if AThread = nil then Exit;
 
 Result:=Remove(AThread);
 if Result then TerminateThread(AThread);
end;
   
{==============================================================================}
{==============================================================================}
{TWinsock2UDPServerBuffer}
constructor TWinsock2UDPServerBuffer.Create(ASize:Integer);
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FActive:=False;
 FData:=nil;
 FSize:=ASize;
 FCount:=0;
 
 if FSize <> 0 then FData:=GetMem(FSize);
end;
 
{==============================================================================}

destructor TWinsock2UDPServerBuffer.Destroy; 
begin
 {}
 AcquireLock;
 try
  if FData <> nil then FreeMem(FData);
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TWinsock2UDPServerBuffer.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TWinsock2UDPServerBuffer.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TWinsock2UDPServerBuffer.GetActive:Boolean;
begin
 {}
 Result:=False;
 
 if not AcquireLock then Exit;

 Result:=FActive;

 ReleaseLock;
end;

{==============================================================================}

procedure TWinsock2UDPServerBuffer.SetActive(AActive:Boolean);
begin
 {}
 if not AcquireLock then Exit;

 FActive:=AActive;

 ReleaseLock;
end;
   
{==============================================================================}
   
procedure TWinsock2UDPServerBuffer.SetCount(ACount:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FCount:=ACount;

 ReleaseLock;
end;
   
{==============================================================================}
{==============================================================================}
{TWinsock2UDPServerBuffers}
constructor TWinsock2UDPServerBuffers.Create(AListener:TWinsock2UDPListener);
begin
 {}
 inherited Create;
 FMin:=5;
 FMax:=10;
 
 FListener:=AListener;
end;

{==============================================================================}

function TWinsock2UDPServerBuffers.GetMin:Integer;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FMin;

 ReleaseLock;
end;

{==============================================================================}

procedure TWinsock2UDPServerBuffers.SetMin(AMin:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FMin:=AMin;
 if FMin = 0 then FMin:=1;
 if FMin > FMax then FMax:=FMin;
 
 ReleaseLock;
end;

{==============================================================================}

function TWinsock2UDPServerBuffers.GetMax:Integer;
begin
 {}
 Result:=0;
 
 if not AcquireLock then Exit;

 Result:=FMax;

 ReleaseLock;
end;

{==============================================================================}

procedure TWinsock2UDPServerBuffers.SetMax(AMax:Integer);
begin
 {}
 if not AcquireLock then Exit;

 FMax:=AMax;
 if FMax < FMin then FMax:=FMin;

 ReleaseLock;
end;

{==============================================================================}

procedure TWinsock2UDPServerBuffers.CreateBuffers;
begin
 {}
 while Count < FMin do
  begin
   if CreateBuffer(False) = nil then Exit;
  end;
end;

{==============================================================================}

function TWinsock2UDPServerBuffers.CreateBuffer(AForce:Boolean):TWinsock2UDPServerBuffer;
begin
 {}
 Result:=nil;
 if FListener = nil then Exit;
  
 if (Count < FMax) or AForce then
  begin
   {Create Buffer}
   if Assigned(FListener.OnCreateBuffer) then
    begin
     FListener.OnCreateBuffer(FListener.BufferSize,Result);
    end;
   if Result = nil then
    begin
     Result:=TWinsock2UDPServerBuffer.Create(FListener.BufferSize);
    end;
   Add(Result);
  end;
end;
   
{==============================================================================}

procedure TWinsock2UDPServerBuffers.DeleteBuffer(ABuffer:TWinsock2UDPServerBuffer);
begin
 {}
 if ABuffer = nil then Exit;

 ABuffer.Free;
end;
 
{==============================================================================}

function TWinsock2UDPServerBuffers.GetBuffer:TWinsock2UDPServerBuffer;
var
 Buffer:TWinsock2UDPServerBuffer;
begin
 {}
 Result:=nil;
 
 if not AcquireLock then Exit;
 try
  Buffer:=TWinsock2UDPServerBuffer(First);
  while Buffer <> nil do
   begin
    if not Buffer.Active then
     begin
      Result:=Buffer;
      Exit;
     end;
     
    Buffer:=TWinsock2UDPServerBuffer(Buffer.Next);
   end;
 finally
  ReleaseLock;
 end;
 
 if Result = nil then Result:=CreateBuffer(True);
end;

{==============================================================================}

procedure TWinsock2UDPServerBuffers.ReleaseBuffer(ABuffer:TWinsock2UDPServerBuffer);
begin
 {}
 if ABuffer = nil then Exit;

 ABuffer.Count:=0; 
 ABuffer.Active:=False;

 if Count > FMax then 
  begin
   Remove(ABuffer);
   DeleteBuffer(ABuffer);
  end;
end;

{==============================================================================}

procedure TWinsock2UDPServerBuffers.DeleteAll;
var
 Buffer:TWinsock2UDPServerBuffer;
begin
 {}
 Buffer:=TWinsock2UDPServerBuffer(First);
 while Buffer <> nil do
  begin
   Remove(Buffer);
   DeleteBuffer(Buffer);
   
   Buffer:=TWinsock2UDPServerBuffer(First);
  end;
end;

{==============================================================================}

function TWinsock2UDPServerBuffers.Delete(ABuffer:TWinsock2UDPServerBuffer):Boolean;
begin
 {}
 Result:=False;
 
 if ABuffer = nil then Exit;
 
 Result:=Remove(ABuffer);
 if Result then DeleteBuffer(ABuffer);
end;
   
{==============================================================================}
{==============================================================================}
{TWinsock2UDPListener}
constructor TWinsock2UDPListener.Create;
begin
 {}
 inherited Create;
 FLock:=CriticalSectionCreate;
 
 FActive:=False;
 FUseListener:=True;
 FThreads:=TWinsock2UDPServerThreads.Create(Self);
 FBuffers:=TWinsock2UDPServerBuffers.Create(Self);
 FListenerThread:=nil;
end;
 
{==============================================================================}
 
destructor TWinsock2UDPListener.Destroy;
begin
 {}
 AcquireLock;
 try
  Active:=False;
  FThreads.TerminateAll;
  FThreads.Free;
  FBuffers.DeleteAll;
  FBuffers.Free;
  inherited Destroy;
 finally
  {ReleaseLock;} {Can destroy Critical Section while holding lock} 
  CriticalSectionDestroy(FLock);
 end;
end;

{==============================================================================}

function TWinsock2UDPListener.AcquireLock:Boolean;
begin
 {}
 Result:=(CriticalSectionLock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}

function TWinsock2UDPListener.ReleaseLock:Boolean;
begin
 {}
 Result:=(CriticalSectionUnlock(FLock) = ERROR_SUCCESS);
end;

{==============================================================================}
  
procedure TWinsock2UDPListener.SetActive(AActive:Boolean);
var
 WorkBool:LongBool;
begin
 {}
 if FActive = AActive then Exit;
 
 if AActive then
  begin
   {Allocate Family}
   if not AllocateFamily then Exit;
   
   {Allocate Socket}
   if AllocateSocket(FSocketType) = INVALID_SOCKET then Exit;
   
   {Set Options}
   if BroadcastEnabled then
    begin
     WorkBool:=BroadcastEnabled;
     if Winsock2.setsockopt(Handle,SOL_SOCKET,SO_BROADCAST,PChar(@WorkBool),SizeOf(WorkBool)) = SOCKET_ERROR then
      begin
       FLastError:=Winsock2.WSAGetLastError;
       Exit;
      end;
    end; 
   
   {Bind}
   if Bind = SOCKET_ERROR then Exit;
   
   {Create Thread}
   FListenerThread:=TWinsock2UDPListenerThread.Create(Self);
   
   {Create Threads}
   FThreads.CreateThreads;
   
   {Create Buffers}
   FBuffers.CreateBuffers;
   
   {Start Thread}
   FListenerThread.Start;
  end
 else
  begin
   {Disconnect Thread}
   FListenerThread.FListener.Disconnect;
   
   {Terminate Thread}
   FListenerThread.TerminateAndWaitFor;
   
   {Terminate Threads}
   FThreads.TerminateAll; 
   
   {Delete Buffers}
   FBuffers.DeleteAll;
  end;
  
 FActive:=AActive;
end;

{==============================================================================}

procedure TWinsock2UDPListener.SetUseListener(AUseListener:Boolean);
begin
 {}
 FLastError:=WSANOTINITIALISED;
 if WS2StartupError <> ERROR_SUCCESS then Exit;

 FUseListener:=AUseListener;
end;

{==============================================================================}

procedure TWinsock2UDPListener.SetLastError(ALastError:LongInt); 
begin
 {}
 FLastError:=ALastError;
end;

{==============================================================================}

function TWinsock2UDPListener.DoExecute(AThread:TWinsock2UDPServerThread):Boolean; 
begin
 {}
 Result:=True;
 if Assigned(FOnExecute) then Result:=FOnExecute(AThread);
end;

{==============================================================================}

function TWinsock2UDPListener.SendToSocketEx(AHandle:THandle;ASockAddr:PSockAddr;ASockLen:Integer;AData:Pointer;ASize:Integer;var ACount:Integer):LongInt;
begin
 {}
 AcquireLock;
 try
  Result:=inherited SendToSocketEx(AHandle,ASockAddr,ASockLen,AData,ASize,ACount);
 finally
  ReleaseLock;
 end; 
end;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure WS2Init;
begin
 {}
 {Check Initialized}
 if WS2Initialized then Exit;
 
 {Set Startup Defaults}
 WS2StartupCount:=0;
 WS2StartupError:=WSANOTINITIALISED;
 
 {Create Startup Lock}
 WS2StartupLock:=CriticalSectionCreate;
 if WS2StartupLock = INVALID_HANDLE_VALUE then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to create winsock2 startup lock');
  end;
 
 {Set TLS Size}
 WS2TlsSize:=SizeOf(TNetToAddr);
 
 {Allocate TLS Index}
 WS2TlsIndex:=ThreadAllocTlsIndexEx(THREAD_TLS_FLAG_FREE);
 if WS2TlsIndex = TLS_OUT_OF_INDEXES then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to allocate TLS index');
  end;
 
 {Set WS2MaxSockets}
 WS2MaxSockets:=WINSOCK2_MAX_SOCKETS;
 
 {Set WS2MaxDatagram}
 WS2MaxDatagram:=WINSOCK2_MAX_UDP;
 
 {Setup Platform Text IO Handlers}
 {TextIOReadCharHandler:=SysTextIOReadChar;}       {Only registered when calling Winsock2RedirectInput}
 {TextIOWriteCharHandler:=SysTextIOWriteChar;}     {Only registered when calling Winsock2RedirectOutput}
 {TextIOWriteBufferHandler:=SysTextIOWriteBuffer;} {Only registered when calling Winsock2RedirectOutput}
 
 WS2Initialized:=True;
end;

{==============================================================================}

function WS2Start:Boolean;
var
 WSAData:TWSAData;
begin
 {}
 Result:=False;
 
 {Start Winsock}
 FillChar(WSAData,SizeOf(TWSAData),0);
 if WSAStartup(WINSOCK_VERSION,WSAData) = ERROR_SUCCESS then
  begin
   {Get Parameters}
   WS2MaxSockets:=WSAData.iMaxSockets;
   WS2MaxDatagram:=WSAData.iMaxUdpDg;
   
   {Return Result}
   Result:=True;
  end; 
end;

{==============================================================================}

function WS2Stop:Boolean;
begin
 {}
 {Stop Winsock}
 Result:=(WSACleanup = NO_ERROR);
end;

{==============================================================================}
 
procedure WS2AsyncStart(Data:Pointer);
begin
 {}
 {Wait for Ready}
 while not(SysInitCompleted) do
  begin
   ThreadSleep(0);
  end;

 {Start Winsock2}
 WS2Start;
end;

{==============================================================================}
{==============================================================================}
{Winsock2 Functions}
function accept( const s: TSocket; addr: PSockAddr; addrlen: PLongint ): TSocket; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=INVALID_SOCKET;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;
  
  {Check Manager}
  if ProtocolManager = nil then Exit;
  
  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Accept Socket}
  Result:=TSocket(Socket.Protocol.Accept(Socket,addr,addrlen));
  
  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=INVALID_SOCKET;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: accept ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function accept( const s: TSocket; addr: PSockAddr; var addrlen: Longint ): TSocket; 
begin
 {}
 Result:=accept(s,addr,@addrlen);
end;

{==============================================================================}

function bind( const s: TSocket; addr: PSockAddr; namelen: Longint ): Longint; 
begin
 {}
 Result:=bind(s,addr^,namelen);
end;

{==============================================================================}

function bind( const s: TSocket; var addr: TSockAddr; namelen: Longint ): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Bind Socket}
  Result:=Socket.Protocol.Bind(Socket,addr,namelen);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: bind ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function closesocket( const s: TSocket ): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Close Socket}
  Result:=Socket.Protocol.CloseSocket(Socket);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: closesocket ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function connect( const s: TSocket; name: PSockAddr; namelen: Longint): Longint; 
begin
 {}
 Result:=connect(s,name^,namelen);
end;

{==============================================================================}

function connect( const s: TSocket; var name: TSockAddr; namelen: Longint): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Connect Socket}
  Result:=Socket.Protocol.Connect(Socket,name,namelen);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: connect ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function ioctlsocket( const s: TSocket; cmd: Longint; var arg: u_long ): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {IOCTL Socket}
  Result:=Socket.Protocol.IoctlSocket(Socket,cmd,arg);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: ioctlsocket ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function ioctlsocket( const s: TSocket; cmd: Longint; argp: pu_long ): Longint; 
begin
 {}
 Result:=ioctlsocket(s,cmd,argp^);
end;

{==============================================================================}

function getpeername( const s: TSocket; var name: TSockAddr; var namelen: Longint ): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Get Socket Peer Name}
  Result:=Socket.Protocol.GetPeerName(Socket,name,namelen);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: getpeername ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getsockname( const s: TSocket; var name: TSockAddr; var namelen: Longint ): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Get Socket Name}
  Result:=Socket.Protocol.GetSockName(Socket,name,namelen);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: getsockname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getsockopt( const s: TSocket; const level, optname: Longint; optval: PChar; var optlen: Longint ): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Get Socket Options}
  Result:=Socket.Protocol.GetSockOpt(Socket,level,optname,optval,optlen);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: getsockopt ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getsockopt( const s: TSocket; const level, optname: Longint; optval: Pointer; var optlen: Longint ): Longint; 
begin
 {}
 Result:=getsockopt(s,level,optname,PChar(optval),optlen);
end;

{==============================================================================}

function getsockopt( const s: TSocket; const level, optname: Longint; var optval; var optlen: Longint ): Longint; 
begin
 {}
 Result:=getsockopt(s,level,optname,PChar(@optval),optlen);
end;

{==============================================================================}

function htonl(hostlong: u_long): u_long;
begin
 {}
 Result:=LongWordNtoBE(hostlong); {Native to Big Endian}
end;

{==============================================================================}

function htons(hostshort: u_short): u_short;
begin
 {}
 Result:=WordNtoBE(hostshort); {Native to Big Endian}
end;

{==============================================================================}

function inet_addr(cp: PChar): u_long;
begin
 {}
 Result:=LongWord(StringToInAddr(cp));
end;

{==============================================================================}

function inet_ntoa(inaddr: TInAddr): PChar; 
{As per the Winsock specification, the buffer returned by this function is only
 guaranteed to be valid until the next Winsock function call is made within the
 same thread. Therefore, the data should be copied before another Winsock call}
var
 WorkBuffer:String;
 NetToAddr:PNetToAddr;
begin
 {}
 Result:=nil;
 
 {Get TLS Value}
 NetToAddr:=ThreadGetTlsValue(WS2TlsIndex);
 if NetToAddr = nil then
  begin
   {Allocate TLS Value}
   NetToAddr:=AllocMem(WS2TlsSize);
   if NetToAddr = nil then Exit;
   
   {Set TLS Value}
   if ThreadSetTlsValue(WS2TlsIndex,NetToAddr) <> ERROR_SUCCESS then Exit;
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

function listen(s: TSocket; backlog: Longint): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: listen ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function ntohl(netlong: u_long): u_long;
begin
 {}
 Result:=LongWordBEtoN(netlong); {Big Endian to Native}
end;

{==============================================================================}

function ntohs(netshort: u_short): u_short; 
begin
 {}
 Result:=WordBEtoN(netshort); {Big Endian to Native}
end;

{==============================================================================}

function recv(s: TSocket; var Buf; len, flags: Longint): Longint;  
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Receive Socket}
  Result:=Socket.Protocol.Recv(Socket,Buf,len,flags);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: recv ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function recv(s: TSocket; Buf: PChar; len, flags: Longint): Longint; 
begin
 {}
 Result:=recv(s,buf^,len,flags);
end;

{==============================================================================}

function recv(s: TSocket; Buf: Pointer; len, flags: Longint): Longint;  
begin
 {}
 Result:=recv(s,buf^,len,flags);
end;

{==============================================================================}

function recvfrom(s: TSocket; Buf: PChar; len, flags: Longint; from: PSockAddr; fromlen: PLongint): Longint; 
begin
 {}
 Result:=recvfrom(s,buf^,len,flags,from^,fromlen^);
end;

{==============================================================================}

function recvfrom(s: TSocket; Buf: Pointer; len, flags: Longint; from: PSockAddr; fromlen: PLongint): Longint; 
begin
 {}
 Result:=recvfrom(s,buf^,len,flags,from^,fromlen^);
end;

{==============================================================================}

function recvfrom(s: TSocket; var Buf; len, flags: Longint; var from: TSockAddr; var fromlen: Longint): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Receive From Socket}
  Result:=Socket.Protocol.RecvFrom(Socket,Buf,len,flags,from,fromlen);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: recvfrom ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function select(nfds: Longint; readfds, writefds, exceptfds: PFDSet; timeout: PTimeVal): Longint; 
{Note: All sockets contained by the FDSet must be of the same type}
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Manager}
  NetworkSetLastError(WSASYSNOTREADY);
  if ProtocolManager = nil then Exit;
  
  {Select Socket}
  Result:=ProtocolManager.Select(nfds,readfds,writefds,exceptfds,timeout);
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: select ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function send(s: TSocket; var Buf; len, flags: Longint): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Send Socket}
  Result:=Socket.Protocol.Send(Socket,Buf,len,flags);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: send ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function send(s: TSocket; Buf: PChar; len, flags: Longint): Longint; 
begin
 {}
 Result:=send(s,Buf^,len,flags);
end;

{==============================================================================}

function send(s: TSocket; Buf: Pointer; len, flags: Longint): Longint; 
begin
 {}
 Result:=send(s,Buf^,len,flags);
end;

{==============================================================================}

function sendto(s: TSocket; var Buf; len, flags: Longint; var addrto: TSockAddr; tolen: Longint): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Send To Socket}
  Result:=Socket.Protocol.SendTo(Socket,Buf,len,flags,addrto,tolen);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: sendto ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function sendto(s: TSocket; Buf: PChar; len, flags: Longint; addrto: PSockAddr; tolen: Longint): Longint; 
begin
 {}
 Result:=sendto(s,buf^,len,flags,addrto^,tolen);
end;

{==============================================================================}

function sendto(s: TSocket; Buf: Pointer; len, flags: Longint; addrto: PSockAddr; tolen: Longint): Longint; 
begin
 {}
 Result:=sendto(s,buf^,len,flags,addrto^,tolen);
end;

{==============================================================================}

function setsockopt(s: TSocket; level, optname: Longint; const optval; optlen: Longint): Longint;  
begin
 {}
 Result:=setsockopt(s,level,optname,PChar(@optval),optlen);
end;

{==============================================================================}

function setsockopt(s: TSocket; level, optname: Longint; optval: PChar; optlen: Longint): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: setsockopt ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function setsockopt(s: TSocket; level, optname: Longint; optval: Pointer; optlen: Longint): Longint; 
begin
 {}
 Result:=setsockopt(s,level,optname,PChar(optval),optlen);
end;

{==============================================================================}

function shutdown(s: TSocket; how: Longint): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: shutdown ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function socket(af, struct, protocol: Longint): TSocket; 
begin
 {}
 Result:=INVALID_SOCKET;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;

  {Check Manager}
  NetworkSetLastError(WSASYSNOTREADY);
  if ProtocolManager = nil then Exit;
  
  {Create Socket}
  Result:=ProtocolManager.Socket(af,Struct,protocol);
 except
  on E: Exception do
   begin
    Result:=INVALID_SOCKET;
    NetworkSetLastError(WSAEPROTONOSUPPORT);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: socket ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function gethostbyaddr(addr: Pointer; len, family: Longint): PHostEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: gethostbyaddr ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function gethostbyname(name: PChar): PHostEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: gethostbyname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function gethostname(name: PChar; len: Longint): Longint; 
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: gethostname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getservbyport(port: Longint; proto: PChar): PServEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: getservbyport ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getservbyname(name, proto: PChar): PServEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: getservbyname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getprotobynumber(proto: Longint): PProtoEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: getprotobynumber ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getprotobyname(name: PChar): PProtoEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: getprotobyname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getaddrinfo(pNodeName, pServiceName: PChar; pHints: PAddrInfo; var ppResult: PAddrInfo): LongInt;
var
 Flags:LongInt;
 Family:LongInt;
 Protocol:LongInt;
 SocketType:LongInt;
 
 Port:Word;
 Addr:PInAddr;
 Host:TInAddr;
 Addr6:PIn6Addr;
 Host6:TIn6Addr;
 HostEnt:PHostEnt;
 ServEnt:PServEnt;
 Current:PAddrInfo;
 WorkBuffer:String;
begin
 {}
 Result:=WSAEAFNOSUPPORT;
 try
  {Set Result}
  ppResult:=nil;
  
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
 
  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;
 
  {Check Names}
  Result:=WSAHOST_NOT_FOUND;
  NetworkSetLastError(WSAHOST_NOT_FOUND);
  if (pNodeName = nil) and (pServiceName = nil) then Exit;
 
  {Check Hints}
  if pHints = nil then
   begin
    Flags:=AI_V4MAPPED or AI_ADDRCONFIG; {Linux defaults / Windows defaults to 0}
    Family:=AF_UNSPEC;
    Protocol:=IPPROTO_IP;
    SocketType:=SOCK_UNSPEC;
   end
  else
   begin
    {Check Hints}
    Result:=WSANO_RECOVERY;
    NetworkSetLastError(WSANO_RECOVERY);
    if pHints.ai_addrlen <> 0 then Exit;
    if pHints.ai_canonname <> nil then Exit;
    if pHints.ai_addr <> nil then Exit;
    if pHints.ai_next <> nil then Exit;
    
    {Get Hints}
    Flags:=pHints.ai_flags;
    Family:=pHints.ai_family;
    Protocol:=pHints.ai_protocol;
    SocketType:=pHints.ai_socktype;
   end;   
  
  {Check Flags (Canonical Name)}
  if (Flags and AI_CANONNAME) = AI_CANONNAME then
   begin
    Result:=WSANO_RECOVERY;
    NetworkSetLastError(WSANO_RECOVERY);
    if pNodeName = nil then Exit;
   end;
  
  {Check Flags (Numeric Host)}
  if (Flags and AI_NUMERICHOST) = AI_NUMERICHOST then
   begin
    Result:=WSAHOST_NOT_FOUND;
    NetworkSetLastError(WSAHOST_NOT_FOUND);
    if pNodeName = nil then Exit;
   end;
  
  {Check Flags (Numeric Service)}  
  if (Flags and AI_NUMERICSERV) = AI_NUMERICSERV then
   begin
    Result:=WSAHOST_NOT_FOUND;
    NetworkSetLastError(WSAHOST_NOT_FOUND);
    if pServiceName = nil then Exit;
   end;
   
  {Check Family}
  Result:=WSAEAFNOSUPPORT;
  NetworkSetLastError(WSAEAFNOSUPPORT);
  if (Family <> AF_UNSPEC) and (Family <> AF_INET) and (Family <> AF_INET6) then Exit;
  if not(IP_TRANSPORT_ENABLED) and (Family = AF_INET) then Exit;   //To Do //Need to use NetworkSettings, restructure when moved to DNSClient 
  if not(IP6_TRANSPORT_ENABLED) and (Family = AF_INET6) then Exit; //To Do //Need to use NetworkSettings, restructure when moved to DNSClient 
  if Family = AF_UNSPEC then
   begin
    if IP_TRANSPORT_ENABLED and not(IP6_TRANSPORT_ENABLED) then Family:=AF_INET;  //To Do //Need to use NetworkSettings, restructure when moved to DNSClient 
    if not(IP_TRANSPORT_ENABLED) and IP6_TRANSPORT_ENABLED then Family:=AF_INET6; //To Do //Need to use NetworkSettings, restructure when moved to DNSClient 
   end;
   
  {Check Socket Type}
  Result:=WSAESOCKTNOSUPPORT;
  NetworkSetLastError(WSAESOCKTNOSUPPORT);
  if (SocketType <> SOCK_UNSPEC) and (SocketType <> SOCK_STREAM) and (SocketType <> SOCK_DGRAM) and (SocketType <> SOCK_RAW) then Exit;
  
  {Check Service Name}
  Port:=IPPORT_ANY;
  if pServiceName <> nil then
   begin
    {Check Flags (Numeric Service)}
    if (Flags and AI_NUMERICSERV) = AI_NUMERICSERV then
     begin
      {Convert Service}
      Result:=WSANO_RECOVERY;
      NetworkSetLastError(WSANO_RECOVERY);
      Port:=StrToIntDef(pServiceName,IPPORT_ANY);
      if Port = IPPORT_ANY then Exit;
     end
    else
     begin
      {Resolve Service}
      WorkBuffer:='TCP';
      if Protocol = IPPROTO_UDP then WorkBuffer:='UDP';
      
      ServEnt:=DNSClient.GetServByName(pServiceName,PChar(WorkBuffer));
      if ServEnt = nil then
       begin
        {Convert Service}
        Result:=WSATYPE_NOT_FOUND;
        NetworkSetLastError(WSATYPE_NOT_FOUND);
        Port:=StrToIntDef(pServiceName,IPPORT_ANY);
        if Port = IPPORT_ANY then Exit;
       end
      else
       begin
        Port:=WordBEtoN(ServEnt.s_port);
       end;
     end;
   end;
  
  {Check Node Name}
  if pNodeName <> nil then
   begin
    {Check Flags (Numeric Host)}
    if (Flags and AI_NUMERICHOST) = AI_NUMERICHOST then
     begin
      {Check Family (AF_INET)}
      if (Family = AF_INET) or (Family = AF_UNSPEC) then
       begin
        {Convert Host}
        Host:=StringToInAddr(pNodeName);
        if (not(InAddrIsDefault(Host)) and not(InAddrIsNone(Host))) or (pNodeName = INET_ADDRSTR_ANY) or (pNodeName = INET_ADDRSTR_BROADCAST) then
         begin
          {Create Address Info}
          Result:=WSA_NOT_ENOUGH_MEMORY;
          NetworkSetLastError(WSA_NOT_ENOUGH_MEMORY); 
          ppResult:=AllocMem(SizeOf(TAddrInfo));
          if ppResult = nil then Exit;
          
          {Update Address Info}
          ppResult.ai_flags:=Flags;
          ppResult.ai_family:=AF_INET; {Family}
          ppResult.ai_socktype:=SocketType;
          ppResult.ai_protocol:=Protocol;
          ppResult.ai_addrlen:=SizeOf(TSockAddr);
          
          {Create Sock Address}
          ppResult.ai_addr:=AllocMem(SizeOf(TSockAddr));
          if ppResult.ai_addr = nil then Exit;
      
          {Return Sock Address}
          ppResult.ai_addr.sin_family:=AF_INET; {Family}
          ppResult.ai_addr.sin_port:=WordNtoBE(Port);
          ppResult.ai_addr.sin_addr:=Host;
         end;
       end;
       
      {Check Family (AF_INET6)}
      if (Family = AF_INET6) or (Family = AF_UNSPEC) then
       begin
        {Convert Host}
        Host6:=StringToIn6Addr(pNodeName);
        if (not In6AddrIsDefault(Host6)) or (pNodeName = INET6_ADDRSTR_INIT) then
         begin
          {Create Address Info}
          Result:=WSA_NOT_ENOUGH_MEMORY;
          NetworkSetLastError(WSA_NOT_ENOUGH_MEMORY); 
          ppResult:=AllocMem(SizeOf(TAddrInfo));
          if ppResult = nil then Exit;
      
          {Update Address Info}
          ppResult.ai_flags:=Flags;
          ppResult.ai_family:=AF_INET6; {Family}
          ppResult.ai_socktype:=SocketType;
          ppResult.ai_protocol:=Protocol;
          ppResult.ai_addrlen:=SizeOf(TSockAddr6);
          
          {Create Sock Address}
          ppResult.ai_addr:=AllocMem(SizeOf(TSockAddr6));
          if ppResult.ai_addr = nil then Exit;
      
          {Return Sock Address}
          PSockAddr6(ppResult.ai_addr).sin6_family:=AF_INET6; {Family}
          PSockAddr6(ppResult.ai_addr).sin6_port:=WordNtoBE(Port);
          PSockAddr6(ppResult.ai_addr).sin6_addr:=Host6;
         end;
       end;       
     end
    else
     begin 
      {Resolve Host}     
      HostEnt:=DNSClient.GetHostByName(pNodeName);
      if HostEnt = nil then
       begin
        {Check Family (AF_INET)}
        if (Family = AF_INET) or (Family = AF_UNSPEC) then
         begin
          {Convert Host}
          Host:=StringToInAddr(pNodeName);
          if (not(InAddrIsDefault(Host)) and not(InAddrIsNone(Host))) or (pNodeName = INET_ADDRSTR_ANY) or (pNodeName = INET_ADDRSTR_BROADCAST) then
           begin
            {Create Address Info}
            Result:=WSA_NOT_ENOUGH_MEMORY;
            NetworkSetLastError(WSA_NOT_ENOUGH_MEMORY); 
            ppResult:=AllocMem(SizeOf(TAddrInfo));
            if ppResult = nil then Exit;
          
            {Update Address Info}
            ppResult.ai_flags:=Flags;
            ppResult.ai_family:=AF_INET; {Family}
            ppResult.ai_socktype:=SocketType;
            ppResult.ai_protocol:=Protocol;
            ppResult.ai_addrlen:=SizeOf(TSockAddr);
          
            {Create Sock Address}
            ppResult.ai_addr:=AllocMem(SizeOf(TSockAddr));
            if ppResult.ai_addr = nil then Exit;
        
            {Return Sock Address}
            ppResult.ai_addr.sin_family:=AF_INET; {Family}
            ppResult.ai_addr.sin_port:=WordNtoBE(Port);
            ppResult.ai_addr.sin_addr:=Host;
           end;
         end;
       
        {Check Family (AF_INET6)}
        if (Family = AF_INET6) or (Family = AF_UNSPEC) then
         begin
          {Convert Host}
          Host6:=StringToIn6Addr(pNodeName);
          if not (In6AddrIsDefault(Host6)) or (pNodeName = INET6_ADDRSTR_INIT) then
           begin
            {Create Address Info}
            Result:=WSA_NOT_ENOUGH_MEMORY;
            NetworkSetLastError(WSA_NOT_ENOUGH_MEMORY); 
            ppResult:=AllocMem(SizeOf(TAddrInfo));
            if ppResult = nil then Exit;
        
            {Update Address Info}
            ppResult.ai_flags:=Flags;
            ppResult.ai_family:=AF_INET6; {Family}
            ppResult.ai_socktype:=SocketType;
            ppResult.ai_protocol:=Protocol;
            ppResult.ai_addrlen:=SizeOf(TSockAddr6);
            
            {Create Sock Address}
            ppResult.ai_addr:=AllocMem(SizeOf(TSockAddr6));
            if ppResult.ai_addr = nil then Exit;
        
            {Return Sock Address}
            PSockAddr6(ppResult.ai_addr).sin6_family:=AF_INET6; {Family}
            PSockAddr6(ppResult.ai_addr).sin6_port:=WordNtoBE(Port);
            PSockAddr6(ppResult.ai_addr).sin6_addr:=Host6;
           end;
         end;       
       end
      else
       begin
        //To do //This is not really right, can't get the info from HostEnt
                //Need to move this whole routine to DNSClient.GetAddrInfo
                              
        {Check Family (AF_INET)}
        if (Family = AF_INET) or (Family = AF_UNSPEC) then
         begin
          {Create Address Info}
          Result:=WSA_NOT_ENOUGH_MEMORY;
          NetworkSetLastError(WSA_NOT_ENOUGH_MEMORY); 
          ppResult:=AllocMem(SizeOf(TAddrInfo));
          if ppResult = nil then Exit;
          
          {Update Address Info}
          ppResult.ai_flags:=Flags;
          ppResult.ai_family:=HostEnt.h_addrtype; {Family}
          ppResult.ai_socktype:=SocketType;
          ppResult.ai_protocol:=Protocol;
          ppResult.ai_addrlen:=SizeOf(TSockAddr);
          
          {Create Sock Address}
          ppResult.ai_addr:=AllocMem(SizeOf(TSockAddr));
          if ppResult.ai_addr = nil then Exit;
        
          {Return Sock Address}
          Addr:=PInAddr(HostEnt.h_addr^);
          ppResult.ai_addr.sin_family:=HostEnt.h_addrtype; {Family}
          ppResult.ai_addr.sin_port:=WordNtoBE(Port);
          ppResult.ai_addr.sin_addr:=Addr^;
         end;
        
        {Check Family (AF_INET6)}
        if (Family = AF_INET6) or (Family = AF_UNSPEC) then
         begin
         
          //To do //DNSClient.GetAddrInfo
          
         end;
       end;
     end;  
     
    NetworkSetLastError(ERROR_SUCCESS); 
    Result:=ERROR_SUCCESS;
   end
  else
   begin
    {Check Family (AF_INET)}
    if (Family = AF_INET) or (Family = AF_UNSPEC) then
     begin
      {Create Address Info}
      Result:=WSA_NOT_ENOUGH_MEMORY;
      NetworkSetLastError(WSA_NOT_ENOUGH_MEMORY); 
      ppResult:=AllocMem(SizeOf(TAddrInfo));
      if ppResult = nil then Exit;
      
      {Update Address Info}
      ppResult.ai_flags:=Flags;
      ppResult.ai_family:=AF_INET; {Family}
      ppResult.ai_socktype:=SocketType;
      ppResult.ai_protocol:=Protocol;
      ppResult.ai_addrlen:=SizeOf(TSockAddr);
      
      {Create Sock Address}
      ppResult.ai_addr:=AllocMem(SizeOf(TSockAddr));
      if ppResult.ai_addr = nil then Exit;
      
      {Check Flags (Passive)}
      if (Flags and AI_PASSIVE) = AI_PASSIVE then
       begin
        {Return Bindable Sock Address}
        ppResult.ai_addr.sin_family:=AF_INET; {Family}
        ppResult.ai_addr.sin_port:=WordNtoBE(Port);
        ppResult.ai_addr.sin_addr.S_addr:=LongWordNtoBE(INADDR_ANY);
       end
      else
       begin
        {Return Connectable Sock Address}
        ppResult.ai_addr.sin_family:=AF_INET; {Family}
        ppResult.ai_addr.sin_port:=WordNtoBE(Port);
        ppResult.ai_addr.sin_addr.S_addr:=LongWordNtoBE(INADDR_LOOPBACK);
       end;     
     end;
     
    {Check Family (AF_INET6)}
    if (Family = AF_INET6) or (Family = AF_UNSPEC) then
     begin
      {Create Address Info}
      Result:=WSA_NOT_ENOUGH_MEMORY;
      NetworkSetLastError(WSA_NOT_ENOUGH_MEMORY); 
      Current:=AllocMem(SizeOf(TAddrInfo));
      if Current = nil then Exit;
      
      {Update Address Info}
      Current.ai_flags:=Flags;
      Current.ai_family:=AF_INET6; {Family}
      Current.ai_socktype:=SocketType;
      Current.ai_protocol:=Protocol;
      Current.ai_addrlen:=SizeOf(TSockAddr6);
      
      {Create Sock Address}
      Current.ai_addr:=AllocMem(SizeOf(TSockAddr6));
      if Current.ai_addr = nil then Exit;
      
      {Check Result}
      if ppResult = nil then ppResult:=Current else ppResult.ai_next:=Current;
      
      {Check Flags (Passive)}
      if (Flags and AI_PASSIVE) = AI_PASSIVE then
       begin
        {Return Bindable Sock Address}
        PSockAddr6(Current.ai_addr).sin6_family:=AF_INET6; {Family}
        PSockAddr6(Current.ai_addr).sin6_port:=WordNtoBE(Port);
        PSockAddr6(Current.ai_addr).sin6_addr:=IN6ADDR_ANY_INIT;
       end
      else
       begin
        {Return Connectable Sock Address}
        PSockAddr6(Current.ai_addr).sin6_family:=AF_INET6; {Family}
        PSockAddr6(Current.ai_addr).sin6_port:=WordNtoBE(Port);
        PSockAddr6(Current.ai_addr).sin6_addr:=IN6ADDR_LOOPBACK_INIT;
       end;     
     end;
     
    NetworkSetLastError(ERROR_SUCCESS); 
    Result:=ERROR_SUCCESS;
   end;   
 except
  on E: Exception do
   begin
    Result:=WSANO_RECOVERY;
    NetworkSetLastError(WSANO_RECOVERY);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: getaddrinfo ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

procedure freeaddrinfo(ai: PAddrInfo);
begin
 {}
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;

  {Check Addr}
  NetworkSetLastError(WSAEFAULT);
  if ai = nil then Exit;
 
  {Check Next}
  if ai.ai_next <> nil then
   begin
    {Free Next}
    freeaddrinfo(ai.ai_next);
   end;
   
  {Free Name}
  if ai.ai_canonname <> nil then
   begin
    FreeMem(ai.ai_canonname);
   end;
   
  {Free Addr}
  if ai.ai_addr <> nil then
   begin
    FreeMem(ai.ai_addr);
   end;
   
  {Free Addr}
  FreeMem(ai);
 
  NetworkSetLastError(ERROR_SUCCESS);
 except
  on E: Exception do
   begin
    NetworkSetLastError(WSANO_RECOVERY);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: freeaddrinfo ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getnameinfo(sa: PSockAddr; salen: Integer; host: PChar; hostlen: DWORD; serv: PChar; servlen: DWORD; flags: Integer): Integer;
var
 HostEnt:PHostEnt;
 ServEnt:PServEnt;
 WorkBuffer:String;
begin
 {}
 Result:=WSAEAFNOSUPPORT;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
 
  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;
 
  {Check Sock}
  Result:=WSAEFAULT;
  NetworkSetLastError(WSAEFAULT);
  if sa = nil then Exit;
  
  {Check Names}
  Result:=WSAHOST_NOT_FOUND;
  NetworkSetLastError(WSAHOST_NOT_FOUND);
  if (host = nil) and (serv = nil) then Exit;

  {Check Name Lengths}
  Result:=WSAEINVAL;
  NetworkSetLastError(WSAEINVAL);
  if (host <> nil) and (hostlen = 0) then Exit;
  if (serv <> nil) and (servlen = 0) then Exit;
  
  {Check Length}
  if salen >= SizeOf(TSockAddr6) then
   begin
    {Check AF_INET6}
    Result:=WSAEAFNOSUPPORT;
    NetworkSetLastError(WSAEAFNOSUPPORT);
    if PSockAddr6(sa).sin6_family <> AF_INET6 then Exit;
    
    {Check Host}
    if host <> nil then
     begin
      {Check Flags}
      if (flags and NI_NUMERICHOST) = NI_NUMERICHOST then
       begin
        {Convert Address}
        WorkBuffer:=In6AddrToString(PSockAddr6(sa).sin6_addr);
        
        {Check Host Length}
        Result:=WSAEINVAL;
        NetworkSetLastError(WSAEINVAL);
        if hostlen < Length(WorkBuffer) then Exit;
        
        StrLCopy(host,PChar(WorkBuffer),hostlen);
       end
      else
       begin      
        {Resolve Address}
        HostEnt:=DNSClient.GetHostByAddr(sa,salen,AF_INET6);
        if HostEnt = nil then
         begin
          Result:=WSAHOST_NOT_FOUND;
          NetworkSetLastError(WSAHOST_NOT_FOUND);
          if (flags and NI_NAMEREQD) = NI_NAMEREQD then Exit; 
          
          {Convert Address}
          WorkBuffer:=In6AddrToString(PSockAddr6(sa).sin6_addr);
          
          {Check Host Length}
          Result:=WSAEINVAL;
          NetworkSetLastError(WSAEINVAL);
          if hostlen < Length(WorkBuffer) then Exit;
        
          StrLCopy(host,PChar(WorkBuffer),hostlen);
         end
        else
         begin
          StrLCopy(host,HostEnt.h_name,hostlen); 
         end;         
        
        //To Do //Check NI_NOFQDN
       end; 
     end;

    {Check Service}
    if serv <> nil then
     begin
      {Check Flags}
      if (flags and NI_NUMERICSERV) = NI_NUMERICSERV then 
       begin
        {Convert Service}
        WorkBuffer:=IntToStr(WordBEtoN(PSockAddr6(sa).sin6_port));
        
        {Check Service Length}
        Result:=WSAEINVAL;
        NetworkSetLastError(WSAEINVAL);
        if servlen < Length(WorkBuffer) then Exit;
        
        StrLCopy(serv,PChar(WorkBuffer),servlen);
       end
      else
       begin
        {Resolve Service}
        WorkBuffer:='TCP';
        if (flags and NI_DGRAM) = NI_DGRAM then WorkBuffer:='UDP';
        
        ServEnt:=DNSClient.GetServByPort(PSockAddr6(sa).sin6_port,PChar(WorkBuffer));
        if ServEnt = nil then
         begin
          {Convert Service}
          WorkBuffer:=IntToStr(WordBEtoN(PSockAddr6(sa).sin6_port));
          
          {Check Service Length}
          Result:=WSAEINVAL;
          NetworkSetLastError(WSAEINVAL);
          if servlen < Length(WorkBuffer) then Exit;
        
          StrLCopy(serv,PChar(WorkBuffer),servlen);
         end
        else
         begin        
          StrLCopy(serv,ServEnt.s_name,servlen); 
         end; 
       end;       
     end;
     
    NetworkSetLastError(ERROR_SUCCESS); 
    Result:=ERROR_SUCCESS;
   end
  else if salen >= SizeOf(TSockAddr) then 
   begin
    {Check AF_INET}
    Result:=WSAEAFNOSUPPORT;
    NetworkSetLastError(WSAEAFNOSUPPORT);
    if PSockAddr(sa).sin_family <> AF_INET then Exit;
    
    {Check Host}
    if host <> nil then
     begin
      {Check Flags}
      if (flags and NI_NUMERICHOST) = NI_NUMERICHOST then
       begin
        {Convert Address}
        WorkBuffer:=InAddrToString(PSockAddr(sa).sin_addr);
        
        {Check Host Length}
        Result:=WSAEINVAL;
        NetworkSetLastError(WSAEINVAL);
        if hostlen < Length(WorkBuffer) then Exit;
        
        StrLCopy(host,PChar(WorkBuffer),hostlen);
       end
      else
       begin      
        {Resolve Address}
        HostEnt:=DNSClient.GetHostByAddr(sa,salen,AF_INET);
        if HostEnt = nil then
         begin
          Result:=WSAHOST_NOT_FOUND;
          NetworkSetLastError(WSAHOST_NOT_FOUND);
          if (flags and NI_NAMEREQD) = NI_NAMEREQD then Exit; 
          
          {Convert Address}
          WorkBuffer:=InAddrToString(PSockAddr(sa).sin_addr);
          
          {Check Host Length}
          Result:=WSAEINVAL;
          NetworkSetLastError(WSAEINVAL);
          if hostlen < Length(WorkBuffer) then Exit;
        
          StrLCopy(host,PChar(WorkBuffer),hostlen);
         end
        else
         begin
          StrLCopy(host,HostEnt.h_name,hostlen); 
         end;         
        
        //To Do //Check NI_NOFQDN
       end; 
     end;

    {Check Service}
    if serv <> nil then
     begin
      {Check Flags}
      if (flags and NI_NUMERICSERV) = NI_NUMERICSERV then 
       begin
        {Convert Service}
        WorkBuffer:=IntToStr(WordBEtoN(PSockAddr(sa).sin_port));
        
        {Check Service Length}
        Result:=WSAEINVAL;
        NetworkSetLastError(WSAEINVAL);
        if servlen < (Length(WorkBuffer) + 1) then Exit;
        
        StrLCopy(serv,PChar(WorkBuffer),servlen);
       end
      else
       begin
        {Resolve Service}
        WorkBuffer:='TCP';
        if (flags and NI_DGRAM) = NI_DGRAM then WorkBuffer:='UDP';
        
        ServEnt:=DNSClient.GetServByPort(PSockAddr(sa).sin_port,PChar(WorkBuffer));
        if ServEnt = nil then
         begin
          {Convert Service}
          WorkBuffer:=IntToStr(WordBEtoN(PSockAddr(sa).sin_port));
        
          {Check Service Length}
          Result:=WSAEINVAL;
          NetworkSetLastError(WSAEINVAL);
          if servlen < (Length(WorkBuffer) + 1) then Exit;
        
          StrLCopy(serv,PChar(WorkBuffer),servlen);
         end
        else
         begin
          StrLCopy(serv,ServEnt.s_name,servlen); 
         end; 
       end;       
     end;

    NetworkSetLastError(ERROR_SUCCESS); 
    Result:=ERROR_SUCCESS;
   end;
 except
  on E: Exception do
   begin
    Result:=WSANO_RECOVERY;
    NetworkSetLastError(WSANO_RECOVERY);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: getnameinfo ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function WSAStartup(wVersionRequired: word; var WSData: TWSAData): Longint;
begin
 {}
 Result:=WSAEFAULT; {SOCKET_ERROR; See Spec}
 try
  {Set Result}
  Result:=WSASYSNOTREADY; {SOCKET_ERROR; See Spec}
  NetworkSetLastError(WSASYSNOTREADY);
  
  {Acquire the Lock}
  if CriticalSectionLock(WS2StartupLock) = ERROR_SUCCESS then
   begin
    try
     {Check Count}
     if WS2StartupCount <> 0 then
      begin
       {$IFDEF WINSOCK2_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: WSAStartup additional call');
       {$ENDIF}
       
       {Check Version}
       Result:=WSAVERNOTSUPPORTED; {SOCKET_ERROR; See Spec}
       NetworkSetLastError(WSAVERNOTSUPPORTED);
       if wVersionRequired < WINSOCK2_LOW_VERSION then Exit;
       if wVersionRequired > WINSOCK2_HIGH_VERSION then Exit;
   
       {$IFDEF WINSOCK2_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: WSAStartup checked version');
       {$ENDIF}
   
       {Increment Count}
       Inc(WS2StartupCount);
   
       {Return Winsock2 Data}
       WSData.wVersion:=Min(WINSOCK2_HIGH_VERSION,wVersionRequired);
       WSData.wHighVersion:=WINSOCK2_HIGH_VERSION;
       WSData.szDescription:=WINSOCK2_NAME + ' (' + WINSOCK2_BUILD_VERSION + ')';
       WSData.szSystemStatus:='';
       WSData.iMaxSockets:=WINSOCK2_MAX_SOCKETS;
       WSData.iMaxUdpDg:=WINSOCK2_MAX_UDP;
       WSData.lpVendorInfo:=nil;
       
       {Check Startup}
       if WS2StartupError <> ERROR_SUCCESS then
        begin
         {Return Result}
         Result:=WS2StartupError; {SOCKET_ERROR; See Spec}
         NetworkSetLastError(WS2StartupError);
        end
       else
        begin
         {Return Result}
         Result:=ERROR_SUCCESS; {NO_ERROR; See Spec}
         NetworkSetLastError(ERROR_SUCCESS);
        end;
      end
     else
      begin
       {$IFDEF WINSOCK2_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: WSAStartup initial call');
       {$ENDIF}
      
       {Check Version}
       Result:=WSAVERNOTSUPPORTED; {SOCKET_ERROR; See Spec}
       NetworkSetLastError(WSAVERNOTSUPPORTED);
       WS2StartupError:=WSAVERNOTSUPPORTED;
       if wVersionRequired < WINSOCK2_LOW_VERSION then Exit;
       if wVersionRequired > WINSOCK2_HIGH_VERSION then Exit;
       {$IFDEF WINSOCK2_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: WSAStartup checked version');
       {$ENDIF}
       
       {Increment Count}
       Inc(WS2StartupCount);

       {Return Winsock2 Data}
       WSData.wVersion:=Min(WINSOCK2_HIGH_VERSION,wVersionRequired);
       WSData.wHighVersion:=WINSOCK2_HIGH_VERSION;
       WSData.szDescription:=WINSOCK2_NAME + ' (' + WINSOCK2_BUILD_VERSION + ')';
       WSData.szSystemStatus:='';
       WSData.iMaxSockets:=WINSOCK2_MAX_SOCKETS;
       WSData.iMaxUdpDg:=WINSOCK2_MAX_UDP;
       WSData.lpVendorInfo:=nil;
    
       {Initialize Components}
       Result:=WSASYSNOTREADY; {SOCKET_ERROR; See Spec}
       NetworkSetLastError(WSASYSNOTREADY);
       WS2StartupError:=WSASYSNOTREADY;
       
       {Start Sockets}
       if SocketsStart <> ERROR_SUCCESS then Exit;
       {$IFDEF WINSOCK2_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: WSAStartup started sockets');
       {$ENDIF}
       
       {$IFDEF WINSOCK2_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: WSAStartup completed');
       {$ENDIF}
       
       {Return Result}
       Result:=ERROR_SUCCESS;  {NO_ERROR; See Spec}
       NetworkSetLastError(ERROR_SUCCESS);
       WS2StartupError:=ERROR_SUCCESS;
      end; 
    finally
     {Release the Lock}
     CriticalSectionUnlock(WS2StartupLock);
    end;
   end;
 except
  on E: Exception do
   begin
    Result:=WSAVERNOTSUPPORTED; {SOCKET_ERROR; See Spec}
    NetworkSetLastError(WSAVERNOTSUPPORTED);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: WSAStartup ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function WSACleanup: Longint; 
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Set Error}
  NetworkSetLastError(WSASYSNOTREADY);
  
  {Acquire the Lock}
  if CriticalSectionLock(WS2StartupLock) = ERROR_SUCCESS then
   begin
    try
     {Check Started}
     NetworkSetLastError(WSANOTINITIALISED);
     if WS2StartupCount = 0 then Exit;
  
     {Decrement Count}
     Dec(WS2StartupCount);
     Result:=NO_ERROR;
     NetworkSetLastError(ERROR_SUCCESS);
     if WS2StartupCount > 0 then Exit;
  
     {$IFDEF WINSOCK2_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: WSACleanup final call');
     {$ENDIF}
  
     {Shutdown and Cleanup}
     Result:=SOCKET_ERROR;
     NetworkSetLastError(WSAENETDOWN);
  
     {Stop Sockets}
     if SocketsStop <> ERROR_SUCCESS then Exit;
     {$IFDEF WINSOCK2_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: WSACleanup stopped sockets');
     {$ENDIF}
  
     {$IFDEF WINSOCK2_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: WSACleanup completed');
     {$ENDIF}
 
     {Return Result}
     Result:=NO_ERROR;
     NetworkSetLastError(ERROR_SUCCESS);
     WS2StartupError:=WSANOTINITIALISED;
    finally
     {Release the Lock}
     CriticalSectionUnlock(WS2StartupLock);
    end;
   end;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSANOTINITIALISED);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: WSACleanup ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

procedure WSASetLastError(iError: Longint); inline;
begin
 {}
 NetworkSetLastError(iError);
end;

{==============================================================================}

function WSAGetLastError: Longint; inline;
begin
 {}
 Result:=NetworkGetLastError;
end;

{==============================================================================}

function WSAIsBlocking: BOOL;
begin
 {}
 {Not Implemented}
 Result:=False;
 {Compatible with Winsock 2}
end;

{==============================================================================}

function WSAUnhookBlockingHook: Longint; 
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
 {Compatible with Winsock 2}
end;

{==============================================================================}

function WSASetBlockingHook(lpBlockFunc: TFarProc): TFarProc;
begin
 {}
 {Not Implemented}
 Result:=nil;
 NetworkSetLastError(WSAEOPNOTSUPP);
 {Return Success to be compatible with Winsock 2}
 {Result:=lpBlockFunc;}
end;

{==============================================================================}

function WSACancelBlockingCall: Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
 {Compatible with Winsock 2}
end;

{==============================================================================}

function WSAAsyncGetServByName(HWindow: HWND; wMsg: u_int; name, proto, buf: PChar; buflen: Longint): THandle;
begin
 {}
 {Not Implemented}
 Result:=0; {As per Spec}
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAsyncGetServByPort( HWindow: HWND; wMsg, port: u_int; proto, buf: PChar; buflen: Longint): THandle;
begin
 {}
 {Not Implemented}
 Result:=0; {As per Spec}
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAsyncGetProtoByName(HWindow: HWND; wMsg: u_int; name, buf: PChar; buflen: Longint): THandle;
begin
 {}
 {Not Implemented}
 Result:=0; {As per Spec}
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAsyncGetProtoByNumber(HWindow: HWND; wMsg: u_int; number: Longint; buf: PChar; buflen: Longint): THandle;
begin
 {}
 {Not Implemented}
 Result:=0; {As per Spec}
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAsyncGetHostByName(HWindow: HWND; wMsg: u_int; name, buf: PChar; buflen: Longint): THandle;
begin
 {}
 {Not Implemented}
 Result:=0; {As per Spec}
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAsyncGetHostByAddr(HWindow: HWND; wMsg: u_int; addr: PChar; len, family: Longint; buf: PChar; buflen: Longint): THandle;
begin
 {}
 {Not Implemented}
 Result:=0; {As per Spec}
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSACancelAsyncRequest(hAsyncTaskHandle: THandle): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAsyncSelect(s: TSocket; HWindow: HWND; wMsg: u_int; lEvent: Longint): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function __WSAFDIsSet(s: TSOcket; var FDSet: TFDSet): Bool;
begin
 {}
 Result:=FD_ISSET(s,FDSet);
end;

{==============================================================================}

function inet_pton(Family: Longint; pszAddrString: PChar; pAddrBuf: Pointer): Longint;
begin
 {}
 Result:=InetPtonA(Family,pszAddrString,pAddrBuf);
end;

{==============================================================================}

function InetPtonA(Family: Longint; pszAddrString: PChar; pAddrBuf: Pointer): Longint;
begin
 {}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEFAULT);
 
 {Check Address}
 if pszAddrString = nil then Exit;
 
 {Check Buffer}
 if pAddrBuf = nil then Exit;
 
 {Check Family}
 NetworkSetLastError(WSAEAFNOSUPPORT);
 case Family of
  AF_INET:begin
    {IPv4}
    PInAddr(pAddrBuf)^:=StringToInAddr(pszAddrString);
    
    //To Do //Check result, if not valid return 0
    
    Result:=1; {As per Spec}
    NetworkSetLastError(ERROR_SUCCESS);
   end;
  AF_INET6:begin
    {IPv6}
    PIn6Addr(pAddrBuf)^:=StringToIn6Addr(pszAddrString);
    
    //To Do //Check result, if not valid return 0
    
    Result:=1; {As per Spec}
    NetworkSetLastError(ERROR_SUCCESS);
   end;  
 end;
end;

{==============================================================================}

function InetPtonW(Family: Longint; pszAddrString: PWideChar; pAddrBuf: Pointer): Longint;
begin
 {}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEFAULT);
 
 {Check Address}
 if pszAddrString = nil then Exit;
 
 {Check Buffer}
 if pAddrBuf = nil then Exit;
 
 {Check Family}
 NetworkSetLastError(WSAEAFNOSUPPORT);
 case Family of
  AF_INET:begin
    {IPv4}
    //To Do
   end;
  AF_INET6:begin
    {IPv6}
    //To Do
   end;  
 end;
end;

{==============================================================================}

function inet_ntop(Family: Longint; pAddr: Pointer; pStringBuf: PChar; StringBufSize: Longint): PChar;
begin
 {}
 Result:=InetNtopA(Family,pAddr,pStringBuf,StringBufSize);
end;

{==============================================================================}

function InetNtopA(Family: Longint; pAddr: Pointer; pStringBuf: PChar; StringBufSize: Longint): PChar;
var
 WorkBuffer:String;
begin
 {}
 Result:=nil;
 NetworkSetLastError(WSA_INVALID_PARAMETER);
 
 {Check Address}
 if pAddr = nil then Exit;
 
 {Check Buffer}
 if pStringBuf = nil then Exit;
  
 {Check Family}
 NetworkSetLastError(WSAEAFNOSUPPORT);
 case Family of
  AF_INET:begin
    {IPv4}
    NetworkSetLastError(WSA_INVALID_PARAMETER);
    if StringBufSize < INET_ADDRSTRLEN then Exit;
    
    WorkBuffer:=InAddrToString(PInAddr(pAddr)^);
    StrLCopy(pStringBuf,PChar(WorkBuffer),StringBufSize);
    
    Result:=pStringBuf;
   end;
  AF_INET6:begin
    {IPv6}
    NetworkSetLastError(WSA_INVALID_PARAMETER);
    if StringBufSize < INET6_ADDRSTRLEN then Exit;
    
    WorkBuffer:=In6AddrToString(PIn6Addr(pAddr)^);
    StrLCopy(pStringBuf,PChar(WorkBuffer),StringBufSize);
    
    Result:=pStringBuf;
   end;  
 end;
end;

{==============================================================================}

function InetNtopW(Family: Longint; pAddr: Pointer; pStringBuf: PWideChar; StringBufSize: Longint): PWideChar;
begin
 {}
 Result:=nil;
 NetworkSetLastError(WSA_INVALID_PARAMETER);
 
 {Check Address}
 if pAddr = nil then Exit;
 
 {Check Buffer}
 if pStringBuf = nil then Exit;
  
 {Check Family}
 NetworkSetLastError(WSAEAFNOSUPPORT);
 case Family of
  AF_INET:begin
    {IPv4}
    //To Do
   end;
  AF_INET6:begin
    {IPv6}
    //To Do
   end;  
 end;
end;

{==============================================================================}

function WSAAccept( s : TSocket; addr : TSockAddr; addrlen : PLongint; lpfnCondition : LPCONDITIONPROC; dwCallbackData : DWORD ): TSocket;
begin
 {}
 {Not Implemented}
 Result:=INVALID_SOCKET;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSACloseEvent( hEvent : WSAEVENT) : WordBool;
begin
 {}
 {Not Implemented}
 Result:=False;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAConnect( s : TSocket; const name : PSockAddr; namelen : Longint; lpCallerData,lpCalleeData : LPWSABUF; lpSQOS,lpGQOS : LPQOS ) : Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSACreateEvent : WSAEVENT; 
begin
 {}
 {Not Implemented}
 Result:=WSA_INVALID_EVENT;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSADuplicateSocketA( s : TSocket; dwProcessId : DWORD; lpProtocolInfo : LPWSAProtocol_InfoA ) : Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSADuplicateSocketW( s : TSocket; dwProcessId : DWORD; lpProtocolInfo : LPWSAProtocol_InfoW ) : Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAEnumNetworkEvents( const s : TSocket; const hEventObject : WSAEVENT; lpNetworkEvents : LPWSANETWORKEVENTS ) :Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAEnumProtocolsA( lpiProtocols : PLongint; lpProtocolBuffer : LPWSAProtocol_InfoA; var lpdwBufferLength : DWORD ) : Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAEnumProtocolsW( lpiProtocols : PLongint; lpProtocolBuffer : LPWSAProtocol_InfoW; var lpdwBufferLength : DWORD ) : Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAEventSelect( s : TSocket; hEventObject : WSAEVENT; lNetworkEvents : LongInt ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAGetOverlappedResult( s : TSocket; lpOverlapped : LPWSAOVERLAPPED; lpcbTransfer : LPDWORD; fWait : BOOL; var lpdwFlags : DWORD ) : WordBool; 
begin
 {}
 {Not Implemented}
 Result:=False;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAGetQosByName( s : TSocket; lpQOSName : LPWSABUF; lpQOS : LPQOS ): WordBool;
begin
 {}
 {Not Implemented}
 Result:=False;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAhtonl( s : TSocket; hostlong : u_long; var lpnetlong : DWORD ): Longint; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Host to Network Long}
  lpnetlong:=LongWordNtoBE(hostlong); {Native to Big Endian}

  {Unlock Socket}
  Socket.ReaderUnlock;
  
  {Return Result}
  NetworkSetLastError(ERROR_SUCCESS);
  Result:=ERROR_SUCCESS;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: bind ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function WSAhtons( s : TSocket; hostshort : u_short; var lpnetshort : WORD ): Longint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Host to Network Short}
  lpnetshort:=WordNtoBE(hostshort); {Native to Big Endian}

  {Unlock Socket}
  Socket.ReaderUnlock;
  
  {Return Result}
  NetworkSetLastError(ERROR_SUCCESS);
  Result:=ERROR_SUCCESS;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: bind ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function WSAIoctl( s : TSocket; dwIoControlCode : DWORD; lpvInBuffer : Pointer; cbInBuffer : DWORD; lpvOutBuffer : Pointer; cbOutBuffer : DWORD; lpcbBytesReturned : LPDWORD; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE ) : Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAJoinLeaf( s : TSocket; name : PSockAddr; namelen : Longint; lpCallerData,lpCalleeData : LPWSABUF; lpSQOS,lpGQOS : LPQOS; dwFlags : DWORD ) : TSocket;
begin
 {}
 {Not Implemented}
 Result:=INVALID_SOCKET;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSANtohl( s : TSocket; netlong : u_long; var lphostlong : DWORD ): Longint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Network to Host Long}
  lphostlong:=LongWordBEtoN(netlong); {Big Endian to Native}

  {Unlock Socket}
  Socket.ReaderUnlock;
  
  {Return Result}
  NetworkSetLastError(ERROR_SUCCESS);
  Result:=ERROR_SUCCESS;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: bind ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function WSANtohs( s : TSocket; netshort : u_short; var lphostshort : WORD ): Longint;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Host to Network Short}
  lphostshort:=WordBEtoN(netshort); {Big Endian to Native}

  {Unlock Socket}
  Socket.ReaderUnlock;
  
  {Return Result}
  NetworkSetLastError(ERROR_SUCCESS);
  Result:=ERROR_SUCCESS;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: bind ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function WSARecv( s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesRecvd : DWORD; var lpFlags : DWORD; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSARecvDisconnect( s : TSocket; lpInboundDisconnectData : LPWSABUF ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSARecvFrom( s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesRecvd : DWORD; var lpFlags : DWORD; lpFrom : PSockAddr; lpFromlen : PLongint; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSARecvMsg( s : TSocket; lpMsg : LPWSAMSG; lpdwNumberOfBytesRecvd : DWORD; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE) : Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAResetEvent( hEvent : WSAEVENT ): WordBool;
begin
 {}
 {Not Implemented}
 Result:=False;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSASend( s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesSent : DWORD; dwFlags : DWORD; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSASendDisconnect( s : TSocket; lpOutboundDisconnectData : LPWSABUF ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSASendTo( s : TSocket; lpBuffers : LPWSABUF; dwBufferCount : DWORD; var lpNumberOfBytesSent : DWORD; dwFlags : DWORD; lpTo : PSockAddr; iTolen : Longint; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSASendMsg( s : TSocket; lpMsg : LPWSAMSG; dwFlags : DWORD; lpNumberOfBytesSent : DWORD; lpOverlapped : LPWSAOVERLAPPED; lpCompletionRoutine : LPWSAOVERLAPPED_COMPLETION_ROUTINE) : Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSASetEvent( hEvent : WSAEVENT ): WordBool;
begin
 {}
 {Not Implemented}
 Result:=False;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSASocketA( af, iType, protocol : Longint; lpProtocolInfo : LPWSAProtocol_InfoA; g : GROUP; dwFlags : DWORD ): TSocket;
begin
 {}
 {Not Implemented}
 Result:=INVALID_SOCKET;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSASocketW( af, iType, protocol : Longint; lpProtocolInfo : LPWSAProtocol_InfoW; g : GROUP; dwFlags : DWORD ): TSocket;
begin
 {}
 {Not Implemented}
 Result:=INVALID_SOCKET;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAWaitForMultipleEvents( cEvents : DWORD; lphEvents : PWSAEVENT; fWaitAll : LongBool; dwTimeout : DWORD; fAlertable : LongBool ): DWORD;
begin
 {}
 {Not Implemented}
 Result:=WSA_WAIT_FAILED;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAddressToStringA( var lpsaAddress : TSockAddr; const dwAddressLength : DWORD; const lpProtocolInfo : LPWSAProtocol_InfoA; const lpszAddressString : PChar; var lpdwAddressStringLength : DWORD ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAddressToStringW( var lpsaAddress : TSockAddr; const dwAddressLength : DWORD; const lpProtocolInfo : LPWSAProtocol_InfoW; const lpszAddressString : PWideChar; var lpdwAddressStringLength : DWORD ): Longint; 
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAStringToAddressA( const AddressString : PChar; const AddressFamily: Longint; const lpProtocolInfo : LPWSAProtocol_InfoA; var lpAddress : TSockAddr; var lpAddressLength : Longint ): Longint; 
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAStringToAddressW( const AddressString : PWideChar; const AddressFamily: Longint; const lpProtocolInfo : LPWSAProtocol_InfoA; var lpAddress : TSockAddr; var lpAddressLength : Longint ): Longint; 
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSALookupServiceBeginA( const lpqsRestrictions : LPWSAQuerySetA; const dwControlFlags : DWORD; lphLookup : PHANDLE ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSALookupServiceBeginW( const lpqsRestrictions : LPWSAQuerySetW; const dwControlFlags : DWORD; lphLookup : PHANDLE ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSALookupServiceNextA( const hLookup : THandle; const dwControlFlags : DWORD; var lpdwBufferLength : DWORD; lpqsResults : LPWSAQuerySetA ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSALookupServiceNextW( const hLookup : THandle; const dwControlFlags : DWORD; var lpdwBufferLength : DWORD; lpqsResults : LPWSAQuerySetW ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSALookupServiceEnd( const hLookup : THandle ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAInstallServiceClassA( const lpServiceClassInfo : LPWSAServiceClassInfoA ) : Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAInstallServiceClassW( const lpServiceClassInfo : LPWSAServiceClassInfoW ) : Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSARemoveServiceClass( const lpServiceClassId : PGUID ) : Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAGetServiceClassInfoA( const lpProviderId : PGUID; const lpServiceClassId : PGUID; var lpdwBufSize : DWORD; lpServiceClassInfo : LPWSAServiceClassInfoA ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAGetServiceClassInfoW( const lpProviderId : PGUID; const lpServiceClassId : PGUID; var lpdwBufSize : DWORD; lpServiceClassInfo : LPWSAServiceClassInfoW ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAEnumNameSpaceProvidersA( var lpdwBufferLength: DWORD; const lpnspBuffer: LPWSANameSpace_InfoA ): Longint; 
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAEnumNameSpaceProvidersW( var lpdwBufferLength: DWORD; const lpnspBuffer: LPWSANameSpace_InfoW ): Longint; 
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAGetServiceClassNameByClassIdA( const lpServiceClassId: PGUID; lpszServiceClassName: PChar; var lpdwBufferLength: DWORD ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAGetServiceClassNameByClassIdW( const lpServiceClassId: PGUID; lpszServiceClassName: PWideChar; var lpdwBufferLength: DWORD ): Longint; 
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSASetServiceA( const lpqsRegInfo: LPWSAQuerySetA; const essoperation: TWSAeSetServiceOp; const dwControlFlags: DWORD ): Longint; 
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSASetServiceW( const lpqsRegInfo: LPWSAQuerySetW; const essoperation: TWSAeSetServiceOp; const dwControlFlags: DWORD ): Longint;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAMakeSyncReply(Buflen, Error: Word): Longint;
begin
 {}
 Result:=MakeLong(Buflen,Error);
end;

{==============================================================================}

function WSAMakeSelectReply(Event, Error: Word): Longint;
begin
 {}
 Result:=MakeLong(Event,Error);
end;

{==============================================================================}

function WSAGetAsyncBuflen(Param: Longint): Word;
begin
 {}
 Result:=LoWord(Param);
end;

{==============================================================================}

function WSAGetAsyncError(Param: Longint): Word;
begin
 {}
 Result:=HiWord(Param);
end;

{==============================================================================}

function WSAGetSelectEvent(Param: Longint): Word;
begin
 {}
 Result:=LoWord(Param);
end;

{==============================================================================}

function WSAGetSelectError(Param: Longint): Word;
begin
 {}
 Result:=HiWord(Param);
end;

{==============================================================================}

procedure FD_CLR(Socket: TSocket; var FDSet: TFDSet);
var
 I: Integer;
begin
 {}
 I:=0;
 while I < FDSet.fd_count do
  begin
   if FDSet.fd_array[I] = Socket then
    begin
     while I < FDSet.fd_count - 1 do
      begin
       FDSet.fd_array[I]:=FDSet.fd_array[I + 1];
       Inc(I);
      end;
      
     Dec(FDSet.fd_count);
     Break;
    end;
    
   Inc(I);
  end;
end;

{==============================================================================}

function FD_ISSET(Socket: TSocket; var FDSet: TFDSet): Boolean;
var
 I:Integer;
begin
 {}
 I:=FDSet.fd_count;
 while I > 0 do
  begin
   Dec(I);
   if FDSet.fd_array[I] = Socket then
    begin
     Result:=True;
     Exit;
    end;
  end;
  
 Result:=False;
end;

{==============================================================================}

procedure FD_SET(Socket: TSocket; var FDSet: TFDSet);
begin
 {}
 if FDSet.fd_count < FD_SETSIZE then
  begin
   FDSet.fd_array[FDSet.fd_count]:=Socket;
   Inc(FDSet.fd_count);
  end;
end;

{==============================================================================}

procedure FD_ZERO(var FDSet: TFDSet);
begin
 {}
 FDSet.fd_count:=0;
end;

{==============================================================================}
{==============================================================================}
{Winsock2 Undocumented Functions}
function WsControl(Proto:DWORD;Action:DWORD;pRequestInfo:Pointer; var pcbRequestInfoLen:DWORD;pResponseInfo:Pointer; var pcbResponseInfoLen:DWORD):Integer; 
var
 Protocol:TNetworkProtocol;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;
  
  NetworkSetLastError(WSAEPROTONOSUPPORT);
  
  //To Do //For those that are documented call WsControlEx with adjusted params
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAEPROTONOSUPPORT);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: WsControl ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getnetbyaddr(addr: Pointer; len, Struct: Integer): PNetEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: getnetbyaddr ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getnetbyname(name: PChar): PNetEnt; 
begin
 {}
 Result:=nil;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: getnetbyname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}
{==============================================================================}
{Winsock2 Enhanced Functions}
function WsControlEx(Proto:DWORD;Action:DWORD;pRequestInfo:Pointer; var pcbRequestInfoLen:DWORD;pResponseInfo:Pointer; var pcbResponseInfoLen:DWORD):Integer; 
var
 Count:LongWord;
 Address:TAddressEntry;
 Adapter:TTransportAdapter;
 Binding:TTransportBinding;
 Protocol:TNetworkProtocol;
 Transport:TNetworkTransport;
 
 IPRoute:TIPRouteEntry;
 IPAddress:TIPAddressEntry;
 IPTransport:TIPTransport;
 
 ARPAddress:TARPAddressEntry;
 ARPTransport:TARPTransport;

 TCPSocket:TTCPSocket;
 UDPSocket:TUDPSocket;
 
 WSAIfRow:PWSAIfRow;
 WSAIfTable:PWSAIfTable;
 WSAIpAddrRow:PWSAIpAddrRow;
 WSAIpAddrTable:PWSAIpAddrTable;
 WSAIpNetRow:PWSAIpNetRow;
 WSAIpNetTable:PWSAIpNetTable;
 WSAIpForwardRow:PWSAIpForwardRow;
 WSAIpForwardTable:PWSAIpForwardTable;

 WSATcpRow:PWSATcpRow;
 WSATcpTable:PWSATcpTable;
 WSAUdpRow:PWSAUdpRow;
 WSAUdpTable:PWSAUdpTable;
 
 WSATTL:LongWord;
 WSAIpStats:PWSAIpStats;
 WSAFixedInfo:PWSAFixedInfo;
 WSAIpAdapterInfo:PWSAIpAdapterInfo;
 WSAIpInterfaceInfo:PWSAIpInterfaceInfo;
 WSAIpAdapterIndexMap:PWSAIpAdapterIndexMap;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WS2StartupError <> ERROR_SUCCESS then Exit;

  {Set Error}
  NetworkSetLastError(WSAEPROTONOSUPPORT);
  
  {Check Proto}
  case Proto of
   IPPROTO_IP:begin
     {Set Error}
     NetworkSetLastError(WSAEOPNOTSUPP);
     
     {Get Transport}
     Transport:=TransportManager.GetTransportByType(AF_INET,PACKET_TYPE_IP,True,NETWORK_LOCK_READ);
     if Transport = nil then Exit;
     try
      {Check Action}
      case Action of
       WSA_GETNUMBEROFINTERFACES:begin
         {Get NumberOfInterfaces}
         NetworkSetLastError(WSAENOBUFS);
         pcbResponseInfoLen:=SizeOf(DWORD);
         if pcbRequestInfoLen < pcbResponseInfoLen then Exit;
         
         NetworkSetLastError(WSAEINVAL);
         if pRequestInfo = nil then Exit;
         
         Count:=0;
         
         {Count Adapters}
         Adapter:=Transport.GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ);
         while Adapter <> nil do
          begin
           Inc(Count);
           
           {Get Next Adapter}
           Adapter:=Transport.GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ);
          end; 
         
         {Return Count}
         PDWORD(pRequestInfo)^:=Count;
         
         {Return Result} 
         Result:=NO_ERROR;
         NetworkSetLastError(ERROR_SUCCESS);
        end;
       WSA_GETIFENTRY:begin
         {Get IfEntry}
         NetworkSetLastError(WSAENOBUFS);
         pcbResponseInfoLen:=SizeOf(TWSAIfRow);
         if pcbRequestInfoLen < pcbResponseInfoLen then Exit;

         NetworkSetLastError(WSAEINVAL);
         WSAIfRow:=PWSAIfRow(pRequestInfo);
         if WSAIfRow = nil then Exit;
         
         NetworkSetLastError(WSAENOTSOCK);
         
         {Scan Adapters} 
         Adapter:=Transport.GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ);
         while Adapter <> nil do
          begin
           {Check Adapter}
           if Adapter.Index = WSAIfRow.dwIndex then 
            begin
             {Get IfRow}
             FillChar(WSAIfRow^,SizeOf(TWSAIfRow),0);
             WSAIfRow.wszName:=Adapter.Name;
             WSAIfRow.dwIndex:=Adapter.Index;
             case Adapter.Adapter.MediaType of
              MEDIA_TYPE_ETHERNET:WSAIfRow.dwType:=WSA_IF_TYPE_ETHERNET_CSMACD;
              MEDIA_TYPE_TOKENRING:WSAIfRow.dwType:=WSA_IF_TYPE_ISO88025_TOKENRING;
              MEDIA_TYPE_IEEE80211:WSAIfRow.dwType:=WSA_IF_TYPE_IEEE80211;
              MEDIA_TYPE_LOOPBACK:WSAIfRow.dwType:=WSA_IF_TYPE_SOFTWARE_LOOPBACK;
              MEDIA_TYPE_PPP:WSAIfRow.dwType:=WSA_IF_TYPE_PPP;
              MEDIA_TYPE_SLIP:WSAIfRow.dwType:=WSA_IF_TYPE_SLIP;
             else  
              WSAIfRow.dwType:=WSA_IF_TYPE_OTHER;
             end;
             WSAIfRow.dwMtu:=Adapter.MTU;
             WSAIfRow.dwPhysAddrLen:=SizeOf(THardwareAddress);
             System.Move(Adapter.Hardware[0],WSAIfRow.bPhysAddr[0],SizeOf(THardwareAddress));
             WSAIfRow.dwAdminStatus:=WSA_IF_ADMIN_STATUS_UP;
             WSAIfRow.dwOperStatus:=WSA_IF_OPER_STATUS_OPERATIONAL;
             Break;
            end;
            
           {Get Next Adapter}
           Adapter:=Transport.GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ);
          end;
         if Adapter = nil then Exit;
         
         {Return Result} 
         Result:=NO_ERROR;
         NetworkSetLastError(ERROR_SUCCESS);
        end;
       WSA_GETIFTABLE:begin
         {Get IfTable}
         Count:=0;
         
         {Count Adapters} 
         Adapter:=Transport.GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ);
         while Adapter <> nil do
          begin
           Inc(Count);
           
           {Get Next Adapter}
           Adapter:=Transport.GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ);
          end;
         
         NetworkSetLastError(WSAENOBUFS);
         pcbResponseInfoLen:=(SizeOf(TWSAIfTable) + (Count * SizeOf(TWSAIfRow))); 
         if pcbRequestInfoLen < pcbResponseInfoLen then Exit;
         
         NetworkSetLastError(WSAEINVAL);
         WSAIfTable:=PWSAIfTable(pRequestInfo);
         if WSAIfTable = nil then Exit;

         WSAIfTable.dwNumEntries:=0;
         
         {Scan Adapters} 
         Adapter:=Transport.GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ);
         while Adapter <> nil do
          begin
           Inc(WSAIfTable.dwNumEntries);
           
           {Get IfRow}
           WSAIfRow:=PWSAIfRow(@WSAIfTable.table[WSAIfTable.dwNumEntries - 1]);
           if WSAIfRow = nil then Exit;
           FillChar(WSAIfRow^,SizeOf(TWSAIfRow),0);
           WSAIfRow.wszName:=Adapter.Name;
           WSAIfRow.dwIndex:=Adapter.Index;
           case Adapter.Adapter.MediaType of
            MEDIA_TYPE_ETHERNET:WSAIfRow.dwType:=WSA_IF_TYPE_ETHERNET_CSMACD;
            MEDIA_TYPE_TOKENRING:WSAIfRow.dwType:=WSA_IF_TYPE_ISO88025_TOKENRING;
            MEDIA_TYPE_IEEE80211:WSAIfRow.dwType:=WSA_IF_TYPE_IEEE80211;
            MEDIA_TYPE_LOOPBACK:WSAIfRow.dwType:=WSA_IF_TYPE_SOFTWARE_LOOPBACK;
            MEDIA_TYPE_PPP:WSAIfRow.dwType:=WSA_IF_TYPE_PPP;
            MEDIA_TYPE_SLIP:WSAIfRow.dwType:=WSA_IF_TYPE_SLIP;
           else  
            WSAIfRow.dwType:=WSA_IF_TYPE_OTHER;
           end;
           WSAIfRow.dwMtu:=Adapter.MTU;
           WSAIfRow.dwPhysAddrLen:=SizeOf(THardwareAddress);
           System.Move(Adapter.Hardware[0],WSAIfRow.bPhysAddr[0],SizeOf(THardwareAddress));
           WSAIfRow.dwAdminStatus:=WSA_IF_ADMIN_STATUS_UP;
           WSAIfRow.dwOperStatus:=WSA_IF_OPER_STATUS_OPERATIONAL;
           
           {Get Next Adapter}
           Adapter:=Transport.GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ);
          end;
          
         {Return Result} 
         Result:=NO_ERROR;
         NetworkSetLastError(ERROR_SUCCESS);
        end;
       WSA_GETIPADDRTABLE:begin
         {Get IpAddrTable}
         Count:=0;
        
         {Count Adapters}
         Adapter:=Transport.GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ);
         while Adapter <> nil do
          begin
           Inc(Count);
           
           {Get Next Adapter}
           Adapter:=Transport.GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ);
          end;
         
         NetworkSetLastError(WSAENOBUFS);
         pcbResponseInfoLen:=(SizeOf(TWSAIpAddrTable) + (Count * SizeOf(TWSAIpAddrRow)));
         if pcbRequestInfoLen < pcbResponseInfoLen then Exit;
         
         NetworkSetLastError(WSAEINVAL);
         WSAIpAddrTable:=PWSAIpAddrTable(pRequestInfo);
         if WSAIpAddrTable = nil then Exit;

         WSAIpAddrTable.dwNumEntries:=0;
         
         {Scan Adapters} //To Do //Change to Bindings
         Adapter:=Transport.GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ);
         while Adapter <> nil do
          begin
           Inc(WSAIpAddrTable.dwNumEntries);
           
           {Get IpAddrRow}
           WSAIpAddrRow:=PWSAIpAddrRow(@WSAIpAddrTable.table[WSAIpAddrTable.dwNumEntries - 1]);
           if WSAIpAddrRow = nil then Exit;
           FillChar(WSAIpAddrRow^,SizeOf(TWSAIpAddrRow),0);
           WSAIpAddrRow.dwAddr:=LongWordNtoBE(TIPTransportAdapter(Adapter).Address.S_addr); 
           WSAIpAddrRow.dwIndex:=TIPTransportAdapter(Adapter).Index;
           WSAIpAddrRow.dwMask:=LongWordNtoBE(TIPTransportAdapter(Adapter).Netmask.S_addr); 
           WSAIpAddrRow.dwBCastAddr:=LongWordNtoBE(TIPTransportAdapter(Adapter).Directed.S_addr);
           WSAIpAddrRow.dwReasmSize:=0;
           
           {Get Next Adapter}
           Adapter:=Transport.GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ);
          end;
          
         {Return Result} 
         Result:=NO_ERROR;
         NetworkSetLastError(ERROR_SUCCESS);
        end;
       WSA_GETIPNETTABLE:begin
         {Get ARP Transport}
         NetworkSetLastError(WSAEOPNOTSUPP);
         ARPTransport:=TARPTransport(TransportManager.GetTransportByType(AF_UNSPEC,PACKET_TYPE_ARP,True,NETWORK_LOCK_READ));
         if ARPTransport = nil then Exit;
         try
          {Get IpNetTable}
          Count:=0;
         
          {Count Addresses}
          ARPAddress:=ARPTransport.GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
          while ARPAddress <> nil do
           begin
            Inc(Count);
            
            {Get Next Address}
            ARPAddress:=ARPTransport.GetAddressByNext(ARPAddress,True,True,NETWORK_LOCK_READ);
           end;
          
          NetworkSetLastError(WSAENOBUFS);
          pcbResponseInfoLen:=(SizeOf(TWSAIpNetTable) + (Count * SizeOf(TWSAIpNetRow)));
          if pcbRequestInfoLen < pcbResponseInfoLen then Exit;

          NetworkSetLastError(WSAEINVAL);
          WSAIpNetTable:=PWSAIpNetTable(pRequestInfo);
          if (WSAIpNetTable = nil) and (pcbRequestInfoLen > 0) then Exit;
          
          WSAIpNetTable.dwNumEntries:=0;
          
          {Scan Addresses} 
          ARPAddress:=ARPTransport.GetAddressByNext(nil,True,False,NETWORK_LOCK_READ);
          while ARPAddress <> nil do
           begin
            Inc(WSAIpNetTable.dwNumEntries);
            
            {Get IpNetRow}
            WSAIpNetRow:=PWSAIpNetRow(@WSAIpNetTable.table[WSAIpNetTable.dwNumEntries - 1]);
            if WSAIpNetRow = nil then Exit;
            FillChar(WSAIpNetRow^,SizeOf(TWSAIpNetRow),0);
            WSAIpNetRow.dwIndex:=WSAIpNetTable.dwNumEntries;
            WSAIpNetRow.dwPhysAddrLen:=SizeOf(THardwareAddress);
            System.Move(ARPAddress.Hardware[0],WSAIpNetRow.bPhysAddr[0],SizeOf(THardwareAddress));
            WSAIpNetRow.dwAddr:=LongWordNtoBE(ARPAddress.Address.S_addr); 
            case ARPAddress.AddressType of
             ADDRESS_TYPE_DYNAMIC:WSAIpNetRow.dwType:=WSA_IPNET_TYPE_DYNAMIC;
             ADDRESS_TYPE_STATIC:WSAIpNetRow.dwType:=WSA_IPNET_TYPE_STATIC;
            else
             begin
              WSAIpNetRow.dwType:=WSA_IPNET_TYPE_OTHER;
             end;
            end; 
          
           {Get Next Address}
           ARPAddress:=ARPTransport.GetAddressByNext(ARPAddress,True,True,NETWORK_LOCK_READ);
          end;
          
          {Return Result} 
          Result:=NO_ERROR;
          NetworkSetLastError(ERROR_SUCCESS);
         finally
          ARPTransport.ReaderUnlock;
         end;
        end; 
       WSA_GETIPFORWARDTABLE:begin
         {Get IP Transport}
         IPTransport:=TIPTransport(Transport);
         
         {Get IpForwardTable}
         Count:=0;
         
         {Count Routes}
         IPRoute:=IPTransport.GetRouteByNext(nil,True,False,NETWORK_LOCK_READ);
         while IPRoute <> nil do
          begin
           Inc(Count);
           
           {Get Next Route}
           IPRoute:=IPTransport.GetRouteByNext(IPRoute,True,True,NETWORK_LOCK_READ);
          end;
         
         NetworkSetLastError(WSAENOBUFS);
         pcbResponseInfoLen:=(SizeOf(TWSAIpForwardTable) + (Count * SizeOf(TWSAIpForwardRow)));
         if pcbRequestInfoLen < pcbResponseInfoLen then Exit;
         
         NetworkSetLastError(WSAEINVAL);
         WSAIpForwardTable:=PWSAIpForwardTable(pRequestInfo);
         if WSAIpForwardTable = nil then Exit;

         WSAIpForwardTable.dwNumEntries:=0;
         
         {Scan Routes}
         IPRoute:=IPTransport.GetRouteByNext(nil,True,False,NETWORK_LOCK_READ);
         while IPRoute <> nil do
          begin
           Inc(WSAIpForwardTable.dwNumEntries);
         
           {Get IpForwardRow}
           WSAIpForwardRow:=PWSAIpForwardRow(@WSAIpForwardTable.table[WSAIpForwardTable.dwNumEntries - 1]);
           if WSAIpForwardRow = nil then Exit;
           FillChar(WSAIpForwardRow^,SizeOf(TWSAIpForwardRow),0);
           WSAIpForwardRow.dwForwardDest:=LongWordNtoBE(IPRoute.Network.S_addr); 
           WSAIpForwardRow.dwForwardMask:=LongWordNtoBE(IPRoute.Netmask.S_addr); 
           WSAIpForwardRow.dwForwardPolicy:=IPRoute.TOS;
           WSAIpForwardRow.dwForwardNextHop:=0;
           WSAIpForwardRow.dwForwardType:=WSA_IPROUTE_TYPE_DIRECT;
           if not IPTransport.CompareAddress(IPRoute.Gateway,IPRoute.Address) then
            begin
             WSAIpForwardRow.dwForwardNextHop:=LongWordNtoBE(IPRoute.Gateway.S_addr); 
             WSAIpForwardRow.dwForwardType:=WSA_IPROUTE_TYPE_INDIRECT;
            end;
           IPAddress:=IPTransport.GetAddressByAddress(IPRoute.Address,True,NETWORK_LOCK_READ);
           if IPAddress <> nil then
            begin
             Adapter:=IPTransport.GetAdapterByAdapter(IPAddress.Adapter,True,NETWORK_LOCK_READ);
             if Adapter <> nil then
              begin
               WSAIpForwardRow.dwForwardIfIndex:=TIPTransportAdapter(Adapter).Index;
               
               Adapter.ReaderUnlock;
              end;
             IPAddress.ReaderUnlock;
            end;
           WSAIpForwardRow.dwForwardProto:=WSA_IPPROTO_LOCAL;
           WSAIpForwardRow.dwForwardAge:=(GetTickCount64 - IPRoute.RouteTime) div MILLISECONDS_PER_SECOND;
           WSAIpForwardRow.dwForwardMetric1:=IPRoute.Metric;
           
           {Get Next Route}
           IPRoute:=IPTransport.GetRouteByNext(IPRoute,True,True,NETWORK_LOCK_READ);
          end;
         
         {Return Result} 
         Result:=NO_ERROR;
         NetworkSetLastError(ERROR_SUCCESS);
        end;       
       WSA_GETTCPTABLE:begin
         {Get TCP Protocol}
         NetworkSetLastError(WSAEOPNOTSUPP);
         Protocol:=ProtocolManager.GetProtocolByType(IPPROTO_TCP,SOCK_STREAM,True,NETWORK_LOCK_READ);
         if Protocol = nil then Exit;
         try
          {Get TcpTable}
          Count:=0;
          
          {Count Sockets}
          TCPSocket:=TTCPSocket(Protocol.GetSocketByNext(nil,True,False,NETWORK_LOCK_READ));
          while TCPSocket <> nil do
           begin
            {Check Family, Struct and Proto}
            if (TCPSocket.Family = AF_INET) and (TCPSocket.Struct = SOCK_STREAM) and (TCPSocket.Proto = IPPROTO_TCP) then
             begin
              Inc(Count);
             end;
           
            {Get Next Socket}
            TCPSocket:=TTCPSocket(Protocol.GetSocketByNext(TCPSocket,True,True,NETWORK_LOCK_READ));
           end;

          NetworkSetLastError(WSAENOBUFS);
          pcbResponseInfoLen:=(SizeOf(TWSATcpTable) + (Count * SizeOf(TWSATcpRow)));
          if pcbRequestInfoLen < pcbResponseInfoLen then Exit;
          
          NetworkSetLastError(WSAEINVAL);
          WSATcpTable:=PWSATcpTable(pRequestInfo);
          if WSATcpTable = nil then Exit;
 
          WSATcpTable.dwNumEntries:=0;
           
          {Scan Sockets}
          TCPSocket:=TTCPSocket(Protocol.GetSocketByNext(nil,True,False,NETWORK_LOCK_READ));
          while TCPSocket <> nil do
           begin
            {Check Family, Struct and Proto}
            if (TCPSocket.Family = AF_INET) and (TCPSocket.Struct = SOCK_STREAM) and (TCPSocket.Proto = IPPROTO_TCP) then
             begin
              Inc(WSATcpTable.dwNumEntries);
              
              {Get TcpRow}
              WSATcpRow:=PWSATcpRow(@WSATcpTable.table[WSATcpTable.dwNumEntries - 1]);
              if WSATcpRow = nil then Exit;
              FillChar(WSATcpRow^,SizeOf(TWSATcpRow),0);
              case TTCPState(TCPSocket.ProtocolState).State of
               TCP_STATE_LISTEN:WSATcpRow.dwState:=WSA_TCP_STATE_LISTEN;
               TCP_STATE_SYNSENT:WSATcpRow.dwState:=WSA_TCP_STATE_SYN_SENT;
               TCP_STATE_SYNREC:WSATcpRow.dwState:=WSA_TCP_STATE_SYN_RCVD;
               TCP_STATE_ESTAB:WSATcpRow.dwState:=WSA_TCP_STATE_ESTAB;
               TCP_STATE_FINWAIT1:WSATcpRow.dwState:=WSA_TCP_STATE_FIN_WAIT1;
               TCP_STATE_FINWAIT2:WSATcpRow.dwState:=WSA_TCP_STATE_FIN_WAIT2;
               TCP_STATE_CLOSWAIT:WSATcpRow.dwState:=WSA_TCP_STATE_CLOSE_WAIT;
               TCP_STATE_CLOSING:WSATcpRow.dwState:=WSA_TCP_STATE_CLOSING;
               TCP_STATE_LASTACK:WSATcpRow.dwState:=WSA_TCP_STATE_LAST_ACK;
               TCP_STATE_TIMEWAIT:WSATcpRow.dwState:=WSA_TCP_STATE_TIME_WAIT;
               TCP_STATE_CLOSED:WSATcpRow.dwState:=WSA_TCP_STATE_CLOSED;
              end;
              WSATcpRow.dwLocalPort:=WordNtoBE(TCPSocket.ProtocolState.LocalPort);
              WSATcpRow.dwRemotePort:=WordNtoBE(TCPSocket.ProtocolState.RemotePort);
              WSATcpRow.dwLocalAddr:=LongWordNtoBE(TIPState(TCPSocket.TransportState).LocalAddress.S_addr);
              WSATcpRow.dwRemoteAddr:=LongWordNtoBE(TIPState(TCPSocket.TransportState).RemoteAddress.S_addr);
             end;
             
            {Get Next Socket}
            TCPSocket:=TTCPSocket(Protocol.GetSocketByNext(TCPSocket,True,True,NETWORK_LOCK_READ));
           end;
          
          {Return Result} 
          Result:=NO_ERROR;
          NetworkSetLastError(ERROR_SUCCESS);
         finally
          Protocol.ReaderUnlock;
         end;
        end;       
       WSA_GETUDPTABLE:begin
         {Get UDP Protocol}
         NetworkSetLastError(WSAEOPNOTSUPP);
         Protocol:=ProtocolManager.GetProtocolByType(IPPROTO_UDP,SOCK_DGRAM,True,NETWORK_LOCK_READ);
         if Protocol = nil then Exit;
         try
          {Get UdpTable}
          Count:=0;
         
          {Count Sockets}
          UDPSocket:=TUDPSocket(Protocol.GetSocketByNext(nil,True,False,NETWORK_LOCK_READ));
          while UDPSocket <> nil do
           begin
            {Check Family, Struct and Proto}
            if (UDPSocket.Family = AF_INET) and (UDPSocket.Struct = SOCK_DGRAM) and (UDPSocket.Proto = IPPROTO_UDP) then
             begin
              Inc(Count);
             end;
           
            {Get Next Socket}
            UDPSocket:=TUDPSocket(Protocol.GetSocketByNext(UDPSocket,True,True,NETWORK_LOCK_READ));
           end;

          NetworkSetLastError(WSAENOBUFS);
          pcbResponseInfoLen:=(SizeOf(TWSAUdpTable) + (Count * SizeOf(TWSAUdpRow)));
          if pcbRequestInfoLen < pcbResponseInfoLen then Exit;
          
          NetworkSetLastError(WSAEINVAL);
          WSAUdpTable:=PWSAUdpTable(pRequestInfo);
          if WSAUdpTable = nil then Exit;
 
          WSAUdpTable.dwNumEntries:=0;
           
          {Scan Sockets}
          UDPSocket:=TUDPSocket(Protocol.GetSocketByNext(nil,True,False,NETWORK_LOCK_READ));
          while UDPSocket <> nil do
           begin
            {Check Family, Struct and Proto}
            if (UDPSocket.Family = AF_INET) and (UDPSocket.Struct = SOCK_DGRAM) and (UDPSocket.Proto = IPPROTO_UDP) then
             begin
              Inc(WSAUdpTable.dwNumEntries);
              
              {Get UdpRow}
              WSAUdpRow:=PWSAUdpRow(@WSAUdpTable.table[WSAUdpTable.dwNumEntries - 1]);
              if WSAUdpRow = nil then Exit;
              FillChar(WSAUdpRow^,SizeOf(TWSAUdpRow),0);
              WSAUdpRow.dwLocalPort:=WordNtoBE(UDPSocket.ProtocolState.LocalPort);
              WSAUdpRow.dwLocalAddr:=LongWordNtoBE(TIPState(UDPSocket.TransportState).LocalAddress.S_addr);
             end;
             
            {Get Next Socket}
            UDPSocket:=TUDPSocket(Protocol.GetSocketByNext(UDPSocket,True,True,NETWORK_LOCK_READ));
           end;
          
          {Return Result} 
          Result:=NO_ERROR;
          NetworkSetLastError(ERROR_SUCCESS);
         finally
          Protocol.ReaderUnlock;
         end;
        end;       
       WSA_GETIPSTATISTICS:begin
         {Get IpStatistics}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;   
       WSA_GETICMPSTATISTICS:begin
         {Get IcmpStatistics}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;   
       WSA_GETTCPSTATISTICS:begin
         {Get TcpStatistics}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;   
       WSA_GETUDPSTATISTICS:begin
         {Get UdpStatistics}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;   
       WSA_SETIFENTRY:begin
         {Set IfEntry}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end; 
       WSA_CREATEIPFORWARDENTRY:begin
         {Create IpForwardEntry}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_SETIPFORWARDENTRY:begin       
         {Set IpForwardEntry}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_DELETEIPFORWARDENTRY:begin       
         {Delete IpForwardEntry}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_SETIPSTATISTICS:begin
         {Get IpStatistics}
         NetworkSetLastError(WSAENOBUFS);
         pcbResponseInfoLen:=SizeOf(TWSAIpStats);
         if pcbRequestInfoLen < pcbResponseInfoLen then Exit;
         
         NetworkSetLastError(WSAEINVAL);
         WSAIpStats:=PWSAIpStats(pRequestInfo);
         if WSAIpStats = nil then Exit;
         
         {Set Forwarding}
         if WSAIpStats.dwForwarding <> WSA_USE_CURRENT_FORWARDING then
          begin
           if WSAIpStats.dwForwarding < WSA_IP_FORWARDING then Exit;
           if WSAIpStats.dwForwarding > WSA_IP_NOT_FORWARDING then Exit;
           TIPTransport(Transport).Forwarding:=WSAIpStats.dwForwarding;
          end;
         
         {Set DefaultTTL}
         if WSAIpStats.dwDefaultTTL <> WSA_USE_CURRENT_TTL then
          begin
           NetworkSetLastError(WSAEINVAL);
           if WSAIpStats.dwDefaultTTL = 0 then Exit;
           if WSAIpStats.dwDefaultTTL > 512 then Exit;
           TIPTransport(Transport).DefaultTTL:=WSAIpStats.dwDefaultTTL;
          end;
          
         {Return Result} 
         Result:=NO_ERROR;
         NetworkSetLastError(ERROR_SUCCESS);
        end;
       WSA_SETIPTTL:begin
         {Get Value}
         NetworkSetLastError(WSAENOBUFS);
         pcbResponseInfoLen:=SizeOf(LongWord);
         if pcbRequestInfoLen < pcbResponseInfoLen then Exit;
         
         NetworkSetLastError(WSAEINVAL);
         WSATTL:=LongWord(pRequestInfo);
         
         {Set DefaultTTL}
         NetworkSetLastError(WSAEINVAL);
         if WSATTL = 0 then Exit;
         if WSATTL > 512 then Exit;
         TIPTransport(Transport).DefaultTTL:=WSATTL;
          
         {Return Result} 
         Result:=NO_ERROR;
         NetworkSetLastError(ERROR_SUCCESS);
        end;
       WSA_CREATEIPNETENTRY:begin
         {Create IpNetEntry}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_SETIPNETENTRY:begin
         {Set IpNetEntry}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_DELETEIPNETENTRY:begin
         {Delete IpNetEntry}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_FLUSHIPNETTABLE:begin
         {Flush IpNetEntry}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_SETTCPENTRY:begin
         {Set TcpEntry}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_GETINTERFACEINFO:begin
         {Get IpInterfaceInfo}
         Count:=0;
         
         {Count Adapters}
         Adapter:=Transport.GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ);
         while Adapter <> nil do
          begin
           Inc(Count);
           
           {Get Next Adapter}
           Adapter:=Transport.GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ);
          end;
         
         NetworkSetLastError(WSAENOBUFS);
         pcbResponseInfoLen:=(SizeOf(TWSAIpInterfaceInfo) + (Count * SizeOf(TWSAIpAdapterIndexMap)));
         if pcbRequestInfoLen < pcbResponseInfoLen then Exit;
         
         NetworkSetLastError(WSAEINVAL);
         WSAIpInterfaceInfo:=PWSAIpInterfaceInfo(pRequestInfo);
         if WSAIpInterfaceInfo = nil then Exit;
         
         WSAIpInterfaceInfo.NumAdapters:=0;
         
         {Scan Adapters} 
         Adapter:=Transport.GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ);
         while Adapter <> nil do
          begin
           Inc(WSAIpInterfaceInfo.NumAdapters);
           
           {Get IpAdapterIndexMap}
           WSAIpAdapterIndexMap:=PWSAIpAdapterIndexMap(@WSAIpInterfaceInfo.Adapter[WSAIpInterfaceInfo.NumAdapters - 1]);
           if WSAIpAdapterIndexMap = nil then Exit;
           WSAIpAdapterIndexMap.Index:=Adapter.Index;
           WSAIpAdapterIndexMap.Name:=Adapter.Name;
           
           {Get Next Adapter}
           Adapter:=Transport.GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ);
          end;
          
         {Return Result} 
         Result:=NO_ERROR;
         NetworkSetLastError(ERROR_SUCCESS);
        end;
       WSA_GETBESTINTERFACE:begin
         {Get BestInterface}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_GETBESTROUTE:begin
         {Get BestRoute}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_GETADAPTERINDEX:begin
         {Get AdapterIndex}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_ADDIPADDRESS:begin
         {Add IPAddress}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_DELETEIPADDRESS:begin
         {Delete IPAddress}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_GETNETWORKPARAMS:begin
         {Get FixedInfo}
         NetworkSetLastError(WSAENOBUFS);
         pcbResponseInfoLen:=SizeOf(TWSAFixedInfo);
         if pcbRequestInfoLen < pcbResponseInfoLen then Exit;

         NetworkSetLastError(WSAEINVAL);
         WSAFixedInfo:=PWSAFixedInfo(pRequestInfo);
         if WSAFixedInfo = nil then Exit;
         
         StrLCopy(WSAFixedInfo.HostName,PChar(Transport.Manager.Settings.HostName),WSA_MAX_HOSTNAME_LEN);
         StrLCopy(WSAFixedInfo.DomainName,PChar(Transport.Manager.Settings.DomainName),WSA_MAX_DOMAIN_NAME_LEN);
         WSAFixedInfo.CurrentDnsServer:=@WSAFixedInfo.DnsServerList;
         WSAFixedInfo.DnsServerList.Next:=nil;
         StrLCopy(WSAFixedInfo.DnsServerList.IpAddress.S,PChar(InAddrToString(InAddrToNetwork(TIPTransport(Transport).Nameservers[0]))),15);
         
         {Return Result} 
         Result:=NO_ERROR;
         NetworkSetLastError(ERROR_SUCCESS);
        end;
       WSA_GETADAPTERSINFO:begin
         {Get IpAdapterInfo}
         Count:=0;
         
         {Count Adapters}
         Adapter:=Transport.GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ);
         while Adapter <> nil do
          begin
           Inc(Count);
           
           {Get Next Adapter}
           Adapter:=Transport.GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ);
          end;
         
         NetworkSetLastError(WSAENOBUFS);
         pcbResponseInfoLen:=(SizeOf(TWSAIpAdapterInfo) * Count);
         if pcbRequestInfoLen < pcbResponseInfoLen then Exit;

         NetworkSetLastError(WSAEINVAL);
         WSAIpAdapterInfo:=PWSAIpAdapterInfo(pRequestInfo);
         if WSAIpAdapterInfo = nil then Exit;
         
         FillChar(WSAIpAdapterInfo^,SizeOf(TWSAIpAdapterInfo),0);
         
         {Scan Adapters} 
         Adapter:=Transport.GetAdapterByNext(nil,True,False,NETWORK_LOCK_READ);
         while Adapter <> nil do
          begin
           {Get IpAdapterInfo}
           WSAIpAdapterInfo.AdapterName:=TIPTransportAdapter(Adapter).Name;
           {WSAIpAdapterInfo.Description:=TIPTransportAdapter(Adapter).Description;} {Not supported}
           WSAIpAdapterInfo.AddressLength:=SizeOf(THardwareAddress);
           System.Move(TIPTransportAdapter(Adapter).Hardware[0],WSAIpAdapterInfo.Address[0],SizeOf(THardwareAddress));
           WSAIpAdapterInfo.Index:=TIPTransportAdapter(Adapter).Index;
           case TIPTransportAdapter(Adapter).Adapter.MediaType of
            MEDIA_TYPE_ETHERNET:WSAIpAdapterInfo.Type_:=WSA_IF_TYPE_ETHERNET_CSMACD;
            MEDIA_TYPE_TOKENRING:WSAIpAdapterInfo.Type_:=WSA_IF_TYPE_ISO88025_TOKENRING;
            MEDIA_TYPE_IEEE80211:WSAIpAdapterInfo.Type_:=WSA_IF_TYPE_IEEE80211;
            MEDIA_TYPE_LOOPBACK:WSAIpAdapterInfo.Type_:=WSA_IF_TYPE_SOFTWARE_LOOPBACK;
            MEDIA_TYPE_PPP:WSAIpAdapterInfo.Type_:=WSA_IF_TYPE_PPP;
            MEDIA_TYPE_SLIP:WSAIpAdapterInfo.Type_:=WSA_IF_TYPE_SLIP;
           else  
            WSAIpAdapterInfo.Type_:=WSA_IF_TYPE_OTHER;
           end;
           WSAIpAdapterInfo.DhcpEnabled:=0;
           if TIPTransportAdapter(Adapter).ConfigType = CONFIG_TYPE_DHCP then WSAIpAdapterInfo.DhcpEnabled:=1;
           WSAIpAdapterInfo.CurrentIpAddress:=@WSAIpAdapterInfo.IpAddressList;
           WSAIpAdapterInfo.IpAddressList.Next:=nil;
           StrLCopy(WSAIpAdapterInfo.IpAddressList.IpAddress.S,PChar(InAddrToString(InAddrToNetwork(TIPTransportAdapter(Adapter).Address))),15);
           StrLCopy(WSAIpAdapterInfo.IpAddressList.IpMask.S,PChar(InAddrToString(InAddrToNetwork(TIPTransportAdapter(Adapter).Netmask))),15);
           WSAIpAdapterInfo.GatewayList.Next:=nil;
           StrLCopy(WSAIpAdapterInfo.GatewayList.IpAddress.S,PChar(InAddrToString(InAddrToNetwork(TIPTransportAdapter(Adapter).Gateway))),15);
           WSAIpAdapterInfo.DhcpServer.Next:=nil;
           StrLCopy(WSAIpAdapterInfo.DhcpServer.IpAddress.S,PChar(InAddrToString(InAddrToNetwork(TIPTransportAdapter(Adapter).Server))),15);
           WSAIpAdapterInfo.LeaseObtained:=TIPTransportAdapter(Adapter).LeaseTime;
           WSAIpAdapterInfo.LeaseExpires:=TIPTransportAdapter(Adapter).ExpiryTime;
           
           {Get Next Adapter}
           Adapter:=Transport.GetAdapterByNext(Adapter,True,True,NETWORK_LOCK_READ);
           
           {Get Next IpAdapterInfo}
           if Adapter <> nil then
            begin
             WSAIpAdapterInfo.Next:=PWSAIpAdapterInfo(PtrUInt(WSAIpAdapterInfo) + SizeOf(TWSAIpAdapterInfo));
             WSAIpAdapterInfo:=WSAIpAdapterInfo.Next;
             
             FillChar(WSAIpAdapterInfo^,SizeOf(TWSAIpAdapterInfo),0);
            end;
          end;
          
         {Return Result} 
         Result:=NO_ERROR;
         NetworkSetLastError(ERROR_SUCCESS);
        end;
       WSA_GETPERADAPTERINFO:begin
         {Get PerAdapterInfo}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI
        end;
       WSA_IPRELEASEADDRESS:begin
         {Get IpAdapterIndexMap}
         NetworkSetLastError(WSAENOBUFS);
         pcbResponseInfoLen:=SizeOf(TWSAIpAdapterIndexMap);
         if pcbRequestInfoLen < pcbResponseInfoLen then Exit;

         NetworkSetLastError(WSAEINVAL);
         WSAIpAdapterIndexMap:=PWSAIpAdapterIndexMap(pRequestInfo);
         if WSAIpAdapterIndexMap = nil then Exit;
         
         {Get Adapter}
         //To Do //IPHLPAPI
          
         {Return Result} 
         Result:=NO_ERROR;
         NetworkSetLastError(ERROR_SUCCESS);
        end;
       WSA_IPRENEWADDRESS:begin
         {Get IpAdapterIndexMap}
         NetworkSetLastError(WSAENOBUFS);
         pcbResponseInfoLen:=SizeOf(TWSAIpAdapterIndexMap);
         if pcbRequestInfoLen < pcbResponseInfoLen then Exit;

         NetworkSetLastError(WSAEINVAL);
         WSAIpAdapterIndexMap:=PWSAIpAdapterIndexMap(pRequestInfo);
         if WSAIpAdapterIndexMap = nil then Exit;
         
         {Get Adapter}
         //To Do //IPHLPAPI
          
         {Return Result} 
         Result:=NO_ERROR;
         NetworkSetLastError(ERROR_SUCCESS);
        end;
       WSA_SENDARP:begin
         {Send ARP}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI

        end;
       WSA_GETRTTANDHOPCOUNT:begin
         {Get RTTAndHopCount}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI

        end;
       WSA_GETFRIENDLYIFINDEX:begin
         {Get FriendlyIfIndex}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI

        end;
       WSA_ENABLEROUTER:begin 
         {Enable Router}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI

        end;
       WSA_UNENABLEROUTER:begin
         {Unenable Router}
         NetworkSetLastError(WSAEINVAL);
         
         //To Do //IPHLPAPI

        end;
      end;
     finally
      Transport.ReaderUnlock;
     end;
    end;
   IPPROTO_IPV6:begin
     {Not Currently Supported}
    end;
  end;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAEPROTONOSUPPORT);
    {$IFDEF WINSOCK2_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock2: Exception: WsControlEx ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}
{==============================================================================}
{RTL Text IO Functions}
function SysTextIOReadChar(var ACh:Char;AUserData:Pointer):Boolean;
{Handler for platform TextIOReadChar function}

{Note: Not intended to be called directly by applications}
var
 Count:Integer;
begin
 {}
 Result:=False;
 
 Count:=Winsock2.recv(WS2TextIOInputSocket,ACh,SizeOf(Char),0);
 if Count = SOCKET_ERROR then
  begin
   Winsock2.closesocket(WS2TextIOInputSocket);
   WS2TextIOInputSocket:=INVALID_SOCKET;
   Exit;
  end
 else
  begin
   if Count = 0 then
    begin
     Winsock2.closesocket(WS2TextIOInputSocket);
     WS2TextIOInputSocket:=INVALID_SOCKET;
     Exit;
    end;
  end;

 Result:=True;  
end;

{==============================================================================}

function SysTextIOWriteChar(ACh:Char;AUserData:Pointer):Boolean;
{Handler for platform TextIOWriteChar function}

{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=(SysTextIOWriteBuffer(@ACh,SizeOf(Char),AUserData) = SizeOf(Char));
end;

{==============================================================================}

function SysTextIOWriteBuffer(ABuffer:PChar;ACount:LongInt;AUserData:Pointer):LongInt;
{Handler for platform TextIOWriteBuffer function}

{Note: Not intended to be called directly by applications}
var
 Count:Integer;
 Total:Integer;
 Offset:Integer;
begin
 {}
 Total:=0;
 Result:=0;
 
 if ABuffer = nil then Exit;
 
 if ACount > 0 then
  begin
   Offset:=0;
   repeat
    Count:=Winsock2.send(WS2TextIOOutputSocket,Pointer(PtrUInt(ABuffer) + PtrUInt(Offset))^,ACount - Offset,0);
    if Count = SOCKET_ERROR then
     begin
      Winsock2.closesocket(WS2TextIOOutputSocket);
      WS2TextIOOutputSocket:=INVALID_SOCKET;
      Exit;
     end
    else
     begin
      if Count = 0 then
       begin
        Winsock2.closesocket(WS2TextIOOutputSocket);
        WS2TextIOOutputSocket:=INVALID_SOCKET;
        Exit;
       end
      else
       begin
        Inc(Total,Count);
        Inc(Offset,Count);
       end;
     end;
   until Offset >= ACount;   
  end;
  
 Result:=Total; 
end;

{==============================================================================}
{==============================================================================}
{Winsock2 Helper Functions}
function Winsock2RedirectInput(s:TSocket):Boolean;
{Redirect standard input to the socket specified by s}
{s: The socket to redirect input to (or INVALID_SOCKET to stop redirection)}
{Return: True if completed successfully or False if an error occurred}

{Note: Redirects the input of the text file Input which also
       redirects the input of Read, ReadLn and the standard C library}
begin
 {}
 Result:=True;
 
 if s = INVALID_SOCKET then
  begin
   {Stop Redirection}
   TextIOReadCharHandler:=nil;
   
   WS2TextIOInputSocket:=INVALID_SOCKET;
  end
 else
  begin
   {Start Redirection}
   TextIOReadCharHandler:=SysTextIOReadChar;
  
   WS2TextIOInputSocket:=s;
  end;  
end;

{==============================================================================}

function Winsock2RedirectOutput(s:TSocket):Boolean; 
{Redirect standard output to the socket specified by s}
{s: The socket to redirect output to (or INVALID_SOCKET to stop redirection)}
{Return: True if completed successfully or False if an error occurred}

{Note: Redirects the output of the text files Output, ErrOutput, StdOut and StdErr
       which also redirects the output of Write, WriteLn and the standard C library}
begin
 {}
 Result:=True;
 
 if s = INVALID_SOCKET then
  begin
   {Stop Redirection}
   TextIOWriteCharHandler:=nil;
   TextIOWriteBufferHandler:=nil;
   
   WS2TextIOOutputSocket:=INVALID_SOCKET;
  end
 else
  begin
   {Start Redirection}
   TextIOWriteCharHandler:=SysTextIOWriteChar;
   TextIOWriteBufferHandler:=SysTextIOWriteBuffer;
  
   WS2TextIOOutputSocket:=s;
  end;  
end;

{==============================================================================}

function Winsock2ErrorToString(AError:LongInt):String;
begin
 {}
 Result:='';
 case LongWord(AError) of   {LongWord for WSA_WAIT_FAILED}
  ERROR_SUCCESS:Result:='ERROR_SUCCESS';

  WSAEINTR:Result:='WSAEINTR';
  WSAEBADF:Result:='WSAEBADF';
  WSAEACCES:Result:='WSAEACCES';
  WSAEFAULT:Result:='WSAEFAULT';
  WSAEINVAL:Result:='WSAEINVAL';
  WSAEMFILE:Result:='WSAEMFILE';

  WSAEWOULDBLOCK:Result:='WSAEWOULDBLOCK';
  WSAEINPROGRESS:Result:='WSAEINPROGRESS';
  WSAEALREADY:Result:='WSAEALREADY';
  WSAENOTSOCK:Result:='WSAENOTSOCK';
  WSAEDESTADDRREQ:Result:='WSAEDESTADDRREQ';
  WSAEMSGSIZE:Result:='WSAEMSGSIZE';
  WSAEPROTOTYPE:Result:='WSAEPROTOTYPE';
  WSAENOPROTOOPT:Result:='WSAENOPROTOOPT';
  WSAEPROTONOSUPPORT:Result:='WSAEPROTONOSUPPORT';
  WSAESOCKTNOSUPPORT:Result:='WSAESOCKTNOSUPPORT';
  WSAEOPNOTSUPP:Result:='WSAEOPNOTSUPP';
  WSAEPFNOSUPPORT:Result:='WSAEPFNOSUPPORT';
  WSAEAFNOSUPPORT:Result:='WSAEAFNOSUPPORT';
  WSAEADDRINUSE:Result:='WSAEADDRINUSE';
  WSAEADDRNOTAVAIL:Result:='WSAEADDRNOTAVAIL';
  WSAENETDOWN:Result:='WSAENETDOWN';
  WSAENETUNREACH:Result:='WSAENETUNREACH';
  WSAENETRESET:Result:='WSAENETRESET';
  WSAECONNABORTED:Result:='WSAECONNABORTED';
  WSAECONNRESET:Result:='WSAECONNRESET';
  WSAENOBUFS:Result:='WSAENOBUFS';
  WSAEISCONN:Result:='WSAEISCONN';
  WSAENOTCONN:Result:='WSAENOTCONN';
  WSAESHUTDOWN:Result:='WSAESHUTDOWN';
  WSAETOOMANYREFS:Result:='WSAETOOMANYREFS';
  WSAETIMEDOUT:Result:='WSAETIMEDOUT';
  WSAECONNREFUSED:Result:='WSAECONNREFUSED';
  WSAELOOP:Result:='WSAELOOP';
  WSAENAMETOOLONG:Result:='WSAENAMETOOLONG';
  WSAEHOSTDOWN:Result:='WSAEHOSTDOWN';
  WSAEHOSTUNREACH:Result:='WSAEHOSTUNREACH';
  WSAENOTEMPTY:Result:='WSAENOTEMPTY';
  WSAEPROCLIM:Result:='WSAEPROCLIM';
  WSAEUSERS:Result:='WSAEUSERS';
  WSAEDQUOT:Result:='WSAEDQUOT';
  WSAESTALE:Result:='WSAESTALE';
  WSAEREMOTE:Result:='WSAEREMOTE';

  WSASYSNOTREADY:Result:='WSASYSNOTREADY';
  WSAVERNOTSUPPORTED:Result:='WSAVERNOTSUPPORTED';
  WSANOTINITIALISED:Result:='WSANOTINITIALISED';
  WSAEDISCON:Result:='WSAEDISCON';
  WSAENOMORE:Result:='WSAENOMORE';
  WSAECANCELLED:Result:='WSAECANCELLED';
  WSAEINVALIDPROCTABLE:Result:='WSAEINVALIDPROCTABLE';
  WSAEINVALIDPROVIDER:Result:='WSAEINVALIDPROVIDER';
  WSAEPROVIDERFAILEDINIT:Result:='WSAEPROVIDERFAILEDINIT';
  WSASYSCALLFAILURE:Result:='WSASYSCALLFAILURE';
  WSASERVICE_NOT_FOUND:Result:='WSASERVICE_NOT_FOUND';
  WSATYPE_NOT_FOUND:Result:='WSATYPE_NOT_FOUND';
  WSA_E_NO_MORE:Result:='WSA_E_NO_MORE';
  WSA_E_CANCELLED:Result:='WSA_E_CANCELLED';
  WSAEREFUSED:Result:='WSAEREFUSED';

  WSAHOST_NOT_FOUND:Result:='WSAHOST_NOT_FOUND';
  WSATRY_AGAIN:Result:='WSATRY_AGAIN';
  WSANO_RECOVERY:Result:='WSANO_RECOVERY';
  WSANO_DATA:Result:='WSANO_DATA';
  {WSANO_ADDRESS:Result:='WSANO_ADDRESS';}

  WSA_IO_PENDING:Result:='WSA_IO_PENDING';
  WSA_IO_INCOMPLETE:Result:='WSA_IO_INCOMPLETE';
  WSA_INVALID_HANDLE:Result:='WSA_INVALID_HANDLE';
  WSA_INVALID_PARAMETER:Result:='WSA_INVALID_PARAMETER';
  WSA_NOT_ENOUGH_MEMORY:Result:='WSA_NOT_ENOUGH_MEMORY';
  WSA_OPERATION_ABORTED:Result:='WSA_OPERATION_ABORTED';
  {WSA_INVALID_EVENT:Result:='WSA_INVALID_EVENT';}
  WSA_MAXIMUM_WAIT_EVENTS:Result:='WSA_MAXIMUM_WAIT_EVENTS';
  WSA_WAIT_FAILED:Result:='WSA_WAIT_FAILED';
  {WSA_WAIT_EVENT_0:Result:='WSA_WAIT_EVENT_0';}
  WSA_WAIT_IO_COMPLETION:Result:='WSA_WAIT_IO_COMPLETION';
  WSA_WAIT_TIMEOUT:Result:='WSA_WAIT_TIMEOUT';
  {WSA_INFINITE:Result:='WSA_INFINITE';}
 end;
end;

{==============================================================================}
{==============================================================================}

initialization
 WS2Init;
 if WINSOCK2_AUTOSTART then
  begin
   if not WINSOCK2_ASYNCSTART then
    begin
     {Start Winsock2}
     WS2Start;
    end
   else
    begin
     {Schedule Worker}
     WorkerSchedule(WINSOCK2_STARTDELAY,TWorkerTask(WS2AsyncStart),nil,nil); {Delay start to allow device initialization}
    end;
  end; 
  
{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
