{
Ultibo Winsock interface unit.

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


Winsock
=======

 Notes: All BSD/Winsock functions that accept an Address or Port expect
        them to be in Network order. All other functions that take an
        Address or Port expect them to be in Host order

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Winsock;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,GlobalSock,Platform,Threads,SysUtils,Classes,UltiboClasses,UltiboUtils,Network,Transport,Protocol,Sockets,
     Loopback,ARP,IP,IPv6,UDP,TCP,ICMP,ICMPv6,IGMP,RAW,DHCP,DNS;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Winsock specific constants}
 WINSOCK_VERSION = $0101;
 
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
 INADDR_ANY       = GlobalSock.INADDR_ANY;
 INADDR_LOOPBACK  = GlobalSock.INADDR_LOOPBACK;
 INADDR_BROADCAST = GlobalSock.INADDR_BROADCAST;
 INADDR_NONE      = GlobalSock.INADDR_NONE;

 IN_CLASSA_NET = GlobalSock.IN_CLASSA_NET;
 IN_CLASSA_NSHIFT = GlobalSock.IN_CLASSA_NSHIFT;
 IN_CLASSA_HOST = GlobalSock.IN_CLASSA_HOST;
 IN_CLASSA_MAX = GlobalSock.IN_CLASSA_MAX;
 IN_CLASSB_NET = GlobalSock.IN_CLASSB_NET;
 IN_CLASSB_NSHIFT = GlobalSock.IN_CLASSB_NSHIFT;
 IN_CLASSB_HOST = GlobalSock.IN_CLASSB_HOST;
 IN_CLASSB_MAX = GlobalSock.IN_CLASSB_MAX;
 IN_CLASSC_NET = GlobalSock.IN_CLASSC_NET;
 IN_CLASSC_NSHIFT = GlobalSock.IN_CLASSC_NSHIFT;
 IN_CLASSC_HOST = GlobalSock.IN_CLASSC_HOST;
 
 WSADESCRIPTION_LEN     =   GlobalSock.WSADESCRIPTION_LEN;
 WSASYS_STATUS_LEN      =   GlobalSock.WSASYS_STATUS_LEN;
 
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

{ This is used instead of -1, since the TSocket type is unsigned.}
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
{ Level number for (get/set)sockopt() to apply to socket itself. }
 SOL_SOCKET      = GlobalSock.SOL_SOCKET;

{ Maximum queue length specifiable by listen. }
 SOMAXCONN       = 5;  {SOMAXCONN       = GlobalSock.SOMAXCONN;} {Lower for Winsock 1.1}

 MSG_OOB         = GlobalSock.MSG_OOB;
 MSG_PEEK        = GlobalSock.MSG_PEEK;
 MSG_DONTROUTE   = GlobalSock.MSG_DONTROUTE;

 MSG_MAXIOVLEN   = GlobalSock.MSG_MAXIOVLEN;

 MSG_PARTIAL     = GlobalSock.MSG_PARTIAL;

{ Define constant based on rfc883, used by gethostbyxxxx() calls. }
 MAXGETHOSTSTRUCT        = GlobalSock.MAXGETHOSTSTRUCT;

{ Define flags to be used with the WSAAsyncSelect() call. }
 FD_READ         = GlobalSock.FD_READ;
 FD_WRITE        = GlobalSock.FD_WRITE;
 FD_OOB          = GlobalSock.FD_OOB;
 FD_ACCEPT       = GlobalSock.FD_ACCEPT;
 FD_CONNECT      = GlobalSock.FD_CONNECT;
 FD_CLOSE        = GlobalSock.FD_CLOSE;

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
 {Constants for WsControlEx}
 WSA_MAX_INTERFACE_NAME_LEN = GlobalSock.WSA_MAX_INTERFACE_NAME_LEN;
 WSA_ANY_SIZE = GlobalSock.WSA_ANY_SIZE;

 WSA_MAX_ADAPTER_NAME = GlobalSock.WSA_MAX_ADAPTER_NAME;

 WSA_MAX_ADAPTER_DESCRIPTION_LENGTH = GlobalSock.WSA_MAX_ADAPTER_DESCRIPTION_LENGTH;
 WSA_MAX_ADAPTER_NAME_LENGTH        = GlobalSock.WSA_MAX_ADAPTER_NAME_LENGTH;
 WSA_MAX_ADAPTER_ADDRESS_LENGTH     = GlobalSock.WSA_MAX_ADAPTER_ADDRESS_LENGTH;
 WSA_MAX_HOSTNAME_LEN               = GlobalSock.WSA_MAX_HOSTNAME_LEN;
 WSA_MAX_DOMAIN_NAME_LEN            = GlobalSock.WSA_MAX_DOMAIN_NAME_LEN;
 WSA_MAX_SCOPE_ID_LEN               = GlobalSock.WSA_MAX_SCOPE_ID_LEN;

 WSA_MAXLEN_IFDESCR         = GlobalSock.WSA_MAXLEN_IFDESCR;
 WSA_MAXLEN_PHYSADDR        = GlobalSock.WSA_MAXLEN_PHYSADDR;

const
 WSA_USE_CURRENT_TTL        = GlobalSock.WSA_USE_CURRENT_TTL;
 WSA_USE_CURRENT_FORWARDING = GlobalSock.WSA_USE_CURRENT_FORWARDING;

const
 WSA_IP_FORWARDING     = GlobalSock.WSA_IP_FORWARDING;
 WSA_IP_NOT_FORWARDING = GlobalSock.WSA_IP_NOT_FORWARDING;

 {WsControlEx Functions}
 WSA_GETNUMBEROFINTERFACES  = GlobalSock.WSA_GETNUMBEROFINTERFACES;
 WSA_GETIFENTRY             = GlobalSock.WSA_GETIFENTRY;
 WSA_GETIFTABLE             = GlobalSock.WSA_GETIFTABLE;
 WSA_GETIPADDRTABLE         = GlobalSock.WSA_GETIPADDRTABLE;
 WSA_GETIPNETTABLE          = GlobalSock.WSA_GETIPNETTABLE;
 WSA_GETIPFORWARDTABLE      = GlobalSock.WSA_GETIPFORWARDTABLE;
 WSA_GETTCPTABLE            = GlobalSock.WSA_GETTCPTABLE;
 WSA_GETUDPTABLE            = GlobalSock.WSA_GETUDPTABLE;

 WSA_GETIPSTATISTICS        = GlobalSock.WSA_GETIPSTATISTICS;
 WSA_GETICMPSTATISTICS      = GlobalSock.WSA_GETICMPSTATISTICS;
 WSA_GETTCPSTATISTICS       = GlobalSock.WSA_GETTCPSTATISTICS;
 WSA_GETUDPSTATISTICS       = GlobalSock.WSA_GETUDPSTATISTICS;

 WSA_SETIFENTRY             = GlobalSock.WSA_SETIFENTRY;
 WSA_CREATEIPFORWARDENTRY   = GlobalSock.WSA_CREATEIPFORWARDENTRY;
 WSA_SETIPFORWARDENTRY      = GlobalSock.WSA_SETIPFORWARDENTRY;
 WSA_DELETEIPFORWARDENTRY   = GlobalSock.WSA_DELETEIPFORWARDENTRY;

 WSA_SETIPSTATISTICS        = GlobalSock.WSA_SETIPSTATISTICS;
 WSA_SETIPTTL               = GlobalSock.WSA_SETIPTTL;

 WSA_CREATEIPNETENTRY       = GlobalSock.WSA_CREATEIPNETENTRY;
 WSA_SETIPNETENTRY          = GlobalSock.WSA_SETIPNETENTRY;
 WSA_DELETEIPNETENTRY       = GlobalSock.WSA_DELETEIPNETENTRY;
 WSA_FLUSHIPNETTABLE        = GlobalSock.WSA_FLUSHIPNETTABLE;

 WSA_CREATEPROXYARPENTRY    = GlobalSock.WSA_CREATEPROXYARPENTRY;
 WSA_DELETEPROXTARPENTRY    = GlobalSock.WSA_DELETEPROXTARPENTRY;

 WSA_SETTCPENTRY            = GlobalSock.WSA_SETTCPENTRY;
 WSA_GETINTERFACEINFO       = GlobalSock.WSA_GETINTERFACEINFO;
 WSA_GETUNIDIRECTIONALADAPTERINFO  = GlobalSock.WSA_GETUNIDIRECTIONALADAPTERINFO;

 WSA_GETBESTINTERFACE       = GlobalSock.WSA_GETBESTINTERFACE;
 WSA_GETBESTROUTE           = GlobalSock.WSA_GETBESTROUTE;
 WSA_NOTIFYADDRCHANGE       = GlobalSock.WSA_NOTIFYADDRCHANGE;
 WSA_NOTIFYROUTECHANGE      = GlobalSock.WSA_NOTIFYROUTECHANGE;
 WSA_GETADAPTERINDEX        = GlobalSock.WSA_GETADAPTERINDEX;
 WSA_ADDIPADDRESS           = GlobalSock.WSA_ADDIPADDRESS;
 WSA_DELETEIPADDRESS        = GlobalSock.WSA_DELETEIPADDRESS;
 WSA_GETNETWORKPARAMS       = GlobalSock.WSA_GETNETWORKPARAMS;
 WSA_GETADAPTERSINFO        = GlobalSock.WSA_GETADAPTERSINFO;
 WSA_GETPERADAPTERINFO      = GlobalSock.WSA_GETPERADAPTERINFO;
 WSA_IPRELEASEADDRESS       = GlobalSock.WSA_IPRELEASEADDRESS;
 WSA_IPRENEWADDRESS         = GlobalSock.WSA_IPRENEWADDRESS;
 WSA_SENDARP                = GlobalSock.WSA_SENDARP;
 WSA_GETRTTANDHOPCOUNT      = GlobalSock.WSA_GETRTTANDHOPCOUNT;
 WSA_GETFRIENDLYIFINDEX     = GlobalSock.WSA_GETFRIENDLYIFINDEX;
 WSA_ENABLEROUTER           = GlobalSock.WSA_ENABLEROUTER;
 WSA_UNENABLEROUTER         = GlobalSock.WSA_UNENABLEROUTER;
 
{==============================================================================}
type
 {Winsock specific types}
 tOS_INT  = GlobalSock.tOS_INT;
 tOS_UINT = GlobalSock.tOS_UINT;
 ptOS_INT = GlobalSock.ptOS_INT;
 ptOS_UINT = GlobalSock.ptOS_UINT;
 u_char = GlobalSock.u_char;
 u_short = GlobalSock.u_short;
 u_int = GlobalSock.u_int;
 u_long = GlobalSock.u_long;
 pu_long = GlobalSock.pu_long;

 TSocket = GlobalSock.TSocket;

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

 PSockAddrIn = GlobalSock.PSockAddrIn;
 sockaddr_in = GlobalSock.sockaddr_in;
 TSockAddrIn = GlobalSock.TSockAddrIn;
 
type
 PWSAData = GlobalSock.PWSAData;
 WSAData = GlobalSock.WSAData;
 TWSAData = GlobalSock.TWSAData;

 PTransmitFileBuffers = GlobalSock.PTransmitFileBuffers;
 _TRANSMIT_FILE_BUFFERS = GlobalSock._TRANSMIT_FILE_BUFFERS;
 TTransmitFileBuffers = GlobalSock.TTransmitFileBuffers;
 TRANSMIT_FILE_BUFFERS = GlobalSock.TRANSMIT_FILE_BUFFERS;
 
type
 { Structure used by kernel to store most addresses. }
 PSOCKADDR = GlobalSock.PSOCKADDR;
 TSockAddr = GlobalSock.TSockAddr;

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
 {Argument structure for IP_ADD_MEMBERSHIP and IP_DROP_MEMBERSHIP}
 PMulticastRequest = GlobalSock.PMulticastRequest;
 ip_mreq = GlobalSock.ip_mreq;
 TMulticastRequest = GlobalSock.TMulticastRequest;
 
type
 {Structures for WsControlEx} {Note: Must be same as IP Helper}
 PWSA_IFROW = GlobalSock.PWSA_IFROW;
 WSA_IFROW = GlobalSock.WSA_IFROW;
 TWSAIfRow = GlobalSock.TWSAIfRow;
 PWSAIfRow = GlobalSock.PWSAIfRow;

 PWSA_IFTABLE = GlobalSock.PWSA_IFTABLE;
 WSA_IFTABLE = GlobalSock.WSA_IFTABLE;
 TWSAIftable = GlobalSock.TWSAIftable;
 PWSAIftable = GlobalSock.PWSAIftable;

 PWSA_IPSTATS = GlobalSock.PWSA_IPSTATS;
 WSA_IPSTATS = GlobalSock.WSA_IPSTATS;
 TWSAIpStats = GlobalSock.TWSAIpStats;
 PWSAIpStats = GlobalSock.PWSAIpStats;

 PWSA_IPADDRROW = GlobalSock.PWSA_IPADDRROW;
 WSA_IPADDRROW = GlobalSock.WSA_IPADDRROW;
 TWSAIpAddrRow = GlobalSock.TWSAIpAddrRow;
 PWSAIpAddrRow = GlobalSock.PWSAIpAddrRow;

 PWSA_IPADDRTABLE = GlobalSock.PWSA_IPADDRTABLE;
 WSA_IPADDRTABLE = GlobalSock.WSA_IPADDRTABLE;
 TWSAIpAddrTable = GlobalSock.TWSAIpAddrTable;
 PWSAIpAddrTable = GlobalSock.PWSAIpAddrTable;

 PWSA_IP_ADAPTER_INDEX_MAP = GlobalSock.PWSA_IP_ADAPTER_INDEX_MAP;
 WSA_IP_ADAPTER_INDEX_MAP = GlobalSock.WSA_IP_ADAPTER_INDEX_MAP;
 TWSAIpAdapterIndexMap = GlobalSock.TWSAIpAdapterIndexMap;
 PWSAIpAdapterIndexMap = GlobalSock.PWSAIpAdapterIndexMap;

 PWSA_IP_INTERFACE_INFO = GlobalSock.PWSA_IP_INTERFACE_INFO;
 WSA_IP_INTERFACE_INFO = GlobalSock.WSA_IP_INTERFACE_INFO;
 TWSAIpInterfaceInfo = GlobalSock.TWSAIpInterfaceInfo;
 PWSAIpInterfaceInfo = GlobalSock.PWSAIpInterfaceInfo;

 PWSA_IP_MASK_STRING = GlobalSock.PWSA_IP_MASK_STRING;
 WSA_IP_ADDRESS_STRING = GlobalSock.WSA_IP_ADDRESS_STRING;
 PWSA_IP_ADDRESS_STRING = GlobalSock.PWSA_IP_ADDRESS_STRING;
 WSA_IP_MASK_STRING = GlobalSock.WSA_IP_MASK_STRING;
 TWSAIpAddressString = GlobalSock.TWSAIpAddressString;
 PWSAIpAddressString = GlobalSock.PWSAIpAddressString;

 PWSA_IP_ADDR_STRING = GlobalSock.PWSA_IP_ADDR_STRING;
 WSA_IP_ADDR_STRING = GlobalSock.WSA_IP_ADDR_STRING;
 TWSAIpAddrString = GlobalSock.TWSAIpAddrString;
 PWSAIpAddrString = GlobalSock.PWSAIpAddrString;

 PWSA_IP_ADAPTER_INFO = GlobalSock.PWSA_IP_ADAPTER_INFO;
 WSA_IP_ADAPTER_INFO = GlobalSock.WSA_IP_ADAPTER_INFO;
 TWSAIpAdapterInfo = GlobalSock.TWSAIpAdapterInfo;
 PWSAIpAdapterInfo = GlobalSock.PWSAIpAdapterInfo;

 PWSA_FIXED_INFO = GlobalSock.PWSA_FIXED_INFO;
 WSA_FIXED_INFO = GlobalSock.WSA_FIXED_INFO;
 TWSAFixedInfo = GlobalSock.TWSAFixedInfo;
 PWSAFixedInfo = GlobalSock.PWSAFixedInfo;
 
{==============================================================================}
{type}
 {Winsock specific classes}
 
{==============================================================================}
{var}
 {Winsock specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure WSInit;
function WSStart:Boolean;
function WSStop:Boolean;

procedure WSAsyncStart(Data:Pointer);

{==============================================================================}
{Winsock Functions}
function accept(s: TSocket; addr: PSockAddr; addrlen : ptOS_INT) : TSocket; overload;
function accept(s: TSocket; addr: PSockAddr; var addrlen : tOS_INT) : TSocket; overload;
function bind(s: TSocket; addr: PSockaddr;namelen:tOS_INT):tOS_INT; overload;
function bind(s: TSocket; var addr: TSockaddr;namelen:tOS_INT):tOS_INT; overload;
function closesocket(s: TSocket):tOS_INT;
function connect(s: TSocket; addr:PSockAddr; namelen:tOS_INT):tOS_INT; overload;
function connect(s: TSocket; var name:TSockAddr; namelen:tOS_INT):tOS_INT; overload;
function ioctlsocket(s: TSocket; cmd:longint; var arg:u_long):tOS_INT; overload;
function ioctlsocket(s: TSocket; cmd:longint; var arg:longint):tOS_INT; overload;
function ioctlsocket(s: TSocket; cmd:longint; argp:pu_long):tOS_INT; overload;
function getpeername(s: TSocket; var name:TSockAddr;var namelen:tOS_INT):tOS_INT;
function getsockname(s: TSocket; var name:TSockAddr;var namelen:tOS_INT):tOS_INT;
function getsockopt(s: TSocket; level:tOS_INT; optname:tOS_INT;optval:pchar;var optlen:tOS_INT):tOS_INT; overload;
function getsockopt(s: TSocket; level:tOS_INT; optname:tOS_INT;optval:pointer;var optlen:tOS_INT):tOS_INT; overload;
function getsockopt(s: TSocket; level:tOS_INT; optname:tOS_INT;var optval;var optlen:tOS_INT):tOS_INT; overload;
function htonl(hostlong: u_long): u_long; 
function htons(hostshort: u_short): u_short; 
function inet_addr(cp: PChar): u_long; 
function inet_ntoa(inaddr: TInAddr): PChar; 
function listen(s: TSocket; backlog:tOS_INT):tOS_INT;
function ntohl(netlong: u_long): u_long; 
function ntohs(netshort: u_short): u_short; 
function recv(s: TSocket;buf:pchar; len:tOS_INT; flags:tOS_INT):tOS_INT; overload;
function recv(s: TSocket;buf:pointer; len:tOS_INT; flags:tOS_INT):tOS_INT; overload;
function recv(s: TSocket;var buf; len:tOS_INT; flags:tOS_INT):tOS_INT; overload;
function recvfrom(s: TSocket;buf:pchar; len:tOS_INT; flags:tOS_INT;from:PSockAddr; fromlen:ptOS_INT):tOS_INT; overload;
function recvfrom(s: TSocket;buf:pointer; len:tOS_INT; flags:tOS_INT;from:PSockAddr; fromlen:ptOS_INT):tOS_INT; overload;
function recvfrom(s: TSocket;var buf; len:tOS_INT; flags:tOS_INT;var from:TSockAddr; var fromlen:tOS_INT):tOS_INT; overload;
function select(nfds:tOS_INT; readfds,writefds,exceptfds : PFDSet;timeout: PTimeVal):tOS_INT;
function send(s: TSocket;var buf; len:tOS_INT; flags:tOS_INT):tOS_INT; overload;
function send(s: TSocket; buf:pchar; len:tOS_INT; flags:tOS_INT):tOS_INT; overload;
function send(s: TSocket;buf:pointer; len:tOS_INT; flags:tOS_INT):tOS_INT; overload;
function sendto(s: TSocket; buf:pchar; len:tOS_INT; flags:tOS_INT;toaddr:PSockAddr; tolen:tOS_INT):tOS_INT; overload;
function sendto(s: TSocket; buf:pointer; len:tOS_INT; flags:tOS_INT;toaddr:PSockAddr; tolen:tOS_INT):tOS_INT; overload;
function sendto(s: TSocket; var buf; len:tOS_INT; flags:tOS_INT;var toaddr:TSockAddr; tolen:tOS_INT):tOS_INT; overload;
function setsockopt(s: TSocket; level:tOS_INT; optname:tOS_INT; optval:pchar; optlen:tOS_INT):tOS_INT; overload;
function setsockopt(s: TSocket; level:tOS_INT; optname:tOS_INT;optval:pointer; optlen:tOS_INT):tOS_INT; overload;
function setsockopt(s: TSocket; level:tOS_INT; optname:tOS_INT; var optval; optlen:tOS_INT):tOS_INT; overload;
function shutdown(s: TSocket; how:tOS_INT):tOS_INT;
function socket(af:tOS_INT; struct:tOS_INT; protocol:tOS_INT):TSocket;

function gethostbyaddr(addr: PChar; len:tOS_INT; family:tOS_INT): PHostEnt;
function gethostbyname(name: PChar): PHostEnt; 
function gethostname(name: PChar; namelen:tOS_INT):tOS_INT;
function getservbyport(port:tOS_INT; proto: PChar):PServEnt;
function getservbyname(name, proto: PChar): PServEnt; 
function getprotobynumber(proto:tOS_INT):PProtoEnt;
function getprotobyname(name: PChar): PProtoEnt; 

function WSAStartup(wVersionRequired:word;var WSAData:TWSADATA):tOS_INT;
function WSACleanup:tOS_INT;
procedure WSASetLastError(iError:tOS_INT); inline;
function WSAGetLastError:tOS_INT; inline;
function WSAIsBlocking: BOOL; 
function WSAUnhookBlockingHook:tOS_INT;
function WSASetBlockingHook(lpBlockFunc: TFarProc): TFarProc; 
function WSACancelBlockingCall:tOS_INT;
function WSAAsyncGetServByName(hWnd:HWND; wMsg:u_int; name:pchar; proto:pchar; buf:pchar; buflen:tOS_INT):THandle;
function WSAAsyncGetServByPort(hWnd:HWND; wMsg:u_int; port:tOS_INT; proto:pchar; buf:pchar; buflen:tOS_INT):THandle;
function WSAAsyncGetProtoByName(hWnd:HWND; wMsg:u_int; name:pchar; buf:pchar; buflen:tOS_INT):THandle;
function WSAAsyncGetProtoByNumber(hWnd:HWND; wMsg:u_int; number:tOS_INT; buf:pchar; buflen:tOS_INT):THandle;
function WSAAsyncGetHostByName(hWnd:HWND; wMsg:u_int; name:pchar; buf:pchar; buflen:tOS_INT):THandle;
function WSAAsyncGetHostByAddr(hWnd:HWND; wMsg:u_int; addr:pchar; len:tOS_INT; family:tOS_INT; buf:pchar; buflen:tOS_INT):THandle;
function WSACancelAsyncRequest(hAsyncTaskHandle:THandle):tOS_INT;
function WSAAsyncSelect(s: TSocket; hWnd:HWND; wMsg:u_int; lEvent:longint):tOS_INT; { really a c-long }
function WSARecvEx(s: TSocket;var buf; len:tOS_INT; flags:ptOS_INT):tOS_INT;
function __WSAFDIsSet(s: TSocket; var FDSet:TFDSet):Bool;
function __WSAFDIsSet_(s: TSocket; var FDSet:TFDSet):tOS_INT;
      
function TransmitFile(hSocket: TSocket; hFile: THandle; nNumberOfBytesToWrite: DWORD; nNumberOfBytesPerSend: DWORD; lpOverlapped: POverlapped; lpTransmitBuffers: PTransmitFileBuffers; dwReserved: DWORD): BOOL; 

function AcceptEx(sListenSocket, sAcceptSocket: TSocket; lpOutputBuffer: Pointer; dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD; var lpdwBytesReceived: DWORD; lpOverlapped: POverlapped): BOOL; 

procedure GetAcceptExSockaddrs(lpOutputBuffer: Pointer; dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD; var LocalSockaddr: PSockAddr; var LocalSockaddrLength: Integer; var RemoteSockaddr: PSockAddr; var RemoteSockaddrLength: Integer); 

function WSAMakeSyncReply(Buflen,Error:Word):dword;
function WSAMakeSelectReply(Event,Error:Word):dword;
function WSAGetAsyncBuflen(Param:dword):Word;
function WSAGetAsyncError(Param:dword):Word;
function WSAGetSelectEvent(Param:dword):Word;
function WSAGetSelectError(Param:dword):Word;

procedure FD_CLR(Socket:TSocket; var FDSet:TFDSet);
function FD_ISSET(Socket:TSocket; var FDSet:TFDSet):Boolean;
procedure FD_SET(Socket:TSocket; var FDSet:TFDSet);
procedure FD_ZERO(var FDSet:TFDSet);

{==============================================================================}
{Winsock Undocumented Functions}
function WsControl(Proto:DWORD;Action:DWORD;pRequestInfo:Pointer; var pcbRequestInfoLen:DWORD;pResponseInfo:Pointer; var pcbResponseInfoLen:DWORD):Integer; 

function getnetbyaddr(addr: Pointer; len, Struct: Integer): PNetEnt; 
function getnetbyname(name: PChar): PNetEnt; 

{==============================================================================}
{Winsock Enhanced Functions}
function WsControlEx(Proto:DWORD;Action:DWORD;pRequestInfo:Pointer; var pcbRequestInfoLen:DWORD;pResponseInfo:Pointer; var pcbResponseInfoLen:DWORD):Integer; 

{==============================================================================}
{RTL Text IO Functions}
function SysTextIOReadChar(var ACh:Char;AUserData:Pointer):Boolean;
function SysTextIOWriteChar(ACh:Char;AUserData:Pointer):Boolean;
function SysTextIOWriteBuffer(ABuffer:PChar;ACount:LongInt;AUserData:Pointer):LongInt;

{==============================================================================}
{Winsock Helper Functions}
function WinsockRedirectInput(s:TSocket):Boolean;
function WinsockRedirectOutput(s:TSocket):Boolean; 

function WinsockErrorToString(AError:LongInt):String;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Winsock specific variables}
 WSInitialized:Boolean;
 
 WSStartupCount:LongWord;
 WSStartupError:LongWord;
 WSStartupLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;

 WSTlsSize:LongWord;
 WSTlsIndex:LongWord;
 
 WSMaxSockets:Word;
 WSMaxDatagram:Word;

 WSTextIOInputSocket:TSocket = INVALID_SOCKET;
 WSTextIOOutputSocket:TSocket = INVALID_SOCKET;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure WSInit;
begin
 {}
 {Check Initialized}
 if WSInitialized then Exit;
 
 {Set Startup Defaults}
 WSStartupCount:=0;
 WSStartupError:=WSANOTINITIALISED;
 
 {Create Startup Lock}
 WSStartupLock:=CriticalSectionCreate;
 if WSStartupLock = INVALID_HANDLE_VALUE then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to create winsock startup lock');
  end;
 
 {Set TLS Size}
 WSTlsSize:=SizeOf(TNetToAddr);
 
 {Allocate TLS Index}
 WSTlsIndex:=ThreadAllocTlsIndexEx(THREAD_TLS_FLAG_FREE);
 if WSTlsIndex = TLS_OUT_OF_INDEXES then
  begin
   if NETWORK_LOG_ENABLED then NetworkLogError(nil,'Failed to allocate TLS index');
  end;
 
 {Set WSMaxSockets}
 WSMaxSockets:=WINSOCK_MAX_SOCKETS;
 
 {Set WSMaxDatagram}
 WSMaxDatagram:=WINSOCK_MAX_UDP;
 
 {Setup Platform Text IO Handlers}
 {TextIOReadCharHandler:=SysTextIOReadChar;}       {Only registered when calling WinsockRedirectInput}
 {TextIOWriteCharHandler:=SysTextIOWriteChar;}     {Only registered when calling WinsockRedirectOutput}
 {TextIOWriteBufferHandler:=SysTextIOWriteBuffer;} {Only registered when calling WinsockRedirectOutput}
 
 WSInitialized:=True;
end;

{==============================================================================}

function WSStart:Boolean;
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
   WSMaxSockets:=WSAData.iMaxSockets;
   WSMaxDatagram:=WSAData.iMaxUdpDg;
   
   {Return Result}
   Result:=True;
  end; 
end;

{==============================================================================}

function WSStop:Boolean;
begin
 {}
 {Stop Winsock}
 Result:=(WSACleanup = NO_ERROR);
end;

{==============================================================================}
 
procedure WSAsyncStart(Data:Pointer);
begin
 {}
 {Wait for Ready}
 while not(SysInitCompleted) do
  begin
   ThreadSleep(0);
  end;

 {Start Winsock}
 WSStart;
end;

{==============================================================================}
{==============================================================================}
{Winsock Functions}
function accept(s: TSocket; addr: PSockAddr; addrlen : ptOS_INT) : TSocket;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=INVALID_SOCKET;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: accept ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function accept(s: TSocket; addr: PSockAddr; var addrlen : tOS_INT) : TSocket;
begin
 {}
 Result:=accept(s,addr,@addrlen);
end;

{==============================================================================}

function bind(s: TSocket; addr: PSockaddr;namelen:tOS_INT):tOS_INT;
begin
 {}
 Result:=bind(s,addr^,namelen);
end;

{==============================================================================}

function bind(s: TSocket; var addr: TSockaddr;namelen:tOS_INT):tOS_INT;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: bind ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function closesocket(s: TSocket):tOS_INT;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: closesocket ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function connect(s: TSocket; addr:PSockAddr; namelen:tOS_INT):tOS_INT;
begin
 {}
 Result:=connect(s,addr^,namelen);
end;

{==============================================================================}

function connect(s: TSocket; var name:TSockAddr; namelen:tOS_INT):tOS_INT;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: connect ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function ioctlsocket(s: TSocket; cmd:longint; var arg:u_long):tOS_INT; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: ioctlsocket ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function ioctlsocket(s: TSocket; cmd:longint; var arg:longint):tOS_INT; 
begin
 {}
 Result:=ioctlsocket(s,cmd,u_long(arg));
end;

{==============================================================================}

function ioctlsocket(s: TSocket; cmd:longint; argp:pu_long):tOS_INT; 
begin
 {}
 Result:=ioctlsocket(s,cmd,argp^);
end;

{==============================================================================}

function getpeername(s: TSocket; var name:TSockAddr;var namelen:tOS_INT):tOS_INT;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: getpeername ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getsockname(s: TSocket; var name:TSockAddr;var namelen:tOS_INT):tOS_INT;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: getsockname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getsockopt(s: TSocket; level:tOS_INT; optname:tOS_INT;optval:pchar;var optlen:tOS_INT):tOS_INT; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: getsockopt ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getsockopt(s: TSocket; level:tOS_INT; optname:tOS_INT;optval:pointer;var optlen:tOS_INT):tOS_INT; 
begin
 {}
 Result:=getsockopt(s,level,optname,PChar(optval),optlen);
end;

{==============================================================================}

function getsockopt(s: TSocket; level:tOS_INT; optname:tOS_INT;var optval;var optlen:tOS_INT):tOS_INT; 
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

function inet_addr(cp: PChar): u_long;  {PInAddr;}  { TInAddr }
begin
 {}
 Result:=LongInt(StringToInAddr(cp));
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
 NetToAddr:=ThreadGetTlsValue(WSTlsIndex);
 if NetToAddr = nil then
  begin
   {Allocate TLS Value}
   NetToAddr:=AllocMem(WSTlsSize);
   if NetToAddr = nil then Exit;
   
   {Set TLS Value}
   if ThreadSetTlsValue(WSTlsIndex,NetToAddr) <> ERROR_SUCCESS then Exit;
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

function listen(s: TSocket; backlog:tOS_INT):tOS_INT;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: listen ' + E.Message);
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

function recv(s: TSocket;buf:pchar; len:tOS_INT; flags:tOS_INT):tOS_INT; 
begin
 {}
 Result:=recv(s,buf^,len,flags);
end;

{==============================================================================}

function recv(s: TSocket;buf:pointer; len:tOS_INT; flags:tOS_INT):tOS_INT; 
begin
 {}
 Result:=recv(s,buf^,len,flags);
end;

{==============================================================================}

function recv(s: TSocket;var buf; len:tOS_INT; flags:tOS_INT):tOS_INT; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: recv ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function recvfrom(s: TSocket;buf:pchar; len:tOS_INT; flags:tOS_INT;from:PSockAddr; fromlen:ptOS_INT):tOS_INT; 
begin
 {}
 Result:=recvfrom(s,buf^,len,flags,from^,fromlen^);
end;

{==============================================================================}

function recvfrom(s: TSocket;buf:pointer; len:tOS_INT; flags:tOS_INT;from:PSockAddr; fromlen:ptOS_INT):tOS_INT; 
begin
 {}
 Result:=recvfrom(s,buf^,len,flags,from^,fromlen^);
end;

{==============================================================================}

function recvfrom(s: TSocket;var buf; len:tOS_INT; flags:tOS_INT;var from:TSockAddr; var fromlen:tOS_INT):tOS_INT; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: recvfrom ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function select(nfds:tOS_INT; readfds,writefds,exceptfds : PFDSet;timeout: PTimeVal):tOS_INT;
{Note: All sockets contained by the FDSet must be of the same type}
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: select ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function send(s: TSocket;var buf; len:tOS_INT; flags:tOS_INT):tOS_INT; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: send ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function send(s: TSocket; buf:pchar; len:tOS_INT; flags:tOS_INT):tOS_INT; 
begin
 {}
 Result:=send(s,buf^,len,flags);
end;

{==============================================================================}

function send(s: TSocket;buf:pointer; len:tOS_INT; flags:tOS_INT):tOS_INT; 
begin
 {}
 Result:=send(s,buf^,len,flags);
end;

{==============================================================================}

function sendto(s: TSocket; buf:pchar; len:tOS_INT; flags:tOS_INT;toaddr:PSockAddr; tolen:tOS_INT):tOS_INT; 
begin
 {}
 Result:=sendto(s,buf^,len,flags,toaddr^,tolen);
end;

{==============================================================================}

function sendto(s: TSocket; buf:pointer; len:tOS_INT; flags:tOS_INT;toaddr:PSockAddr; tolen:tOS_INT):tOS_INT; 
begin
 {}
 Result:=sendto(s,buf^,len,flags,toaddr^,tolen);
end;

{==============================================================================}

function sendto(s: TSocket; var buf; len:tOS_INT; flags:tOS_INT;var toaddr:TSockAddr; tolen:tOS_INT):tOS_INT; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
  {Check Socket}
  NetworkSetLastError(WSAENOTSOCK);
  Socket:=TProtocolSocket(s);
  if Socket = nil then Exit;

  {Check Manager}
  if ProtocolManager = nil then Exit;

  {Check Socket}
  if not ProtocolManager.CheckSocket(s,True,NETWORK_LOCK_READ) then Exit;

  {Send To Socket}
  Result:=Socket.Protocol.SendTo(Socket,Buf,len,flags,toaddr,tolen);

  {Unlock Socket}
  Socket.ReaderUnlock;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAENOTSOCK);
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: sendto ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function setsockopt(s: TSocket; level:tOS_INT; optname:tOS_INT; optval:pchar; optlen:tOS_INT):tOS_INT; 
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: setsockopt ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function setsockopt(s: TSocket; level:tOS_INT; optname:tOS_INT;optval:pointer; optlen:tOS_INT):tOS_INT; 
begin
 {}
 Result:=setsockopt(s,level,optname,PChar(optval),optlen)
end;

{==============================================================================}

function setsockopt(s: TSocket; level:tOS_INT; optname:tOS_INT; var optval; optlen:tOS_INT):tOS_INT; 
begin
 {}
 Result:=setsockopt(s,level,optname,PChar(@optval),optlen)
end;

{==============================================================================}

function shutdown(s: TSocket; how:tOS_INT):tOS_INT;
var
 Socket:TProtocolSocket;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: shutdown ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function socket(af:tOS_INT; struct:tOS_INT; protocol:tOS_INT):TSocket;
begin
 {}
 Result:=INVALID_SOCKET;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: socket ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function gethostbyaddr(addr:pchar; len:tOS_INT; family:tOS_INT): PHostEnt;
begin
 {}
 Result:=nil;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: gethostbyaddr ' + E.Message);
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
  if WSStartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: gethostbyname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function gethostname(name: PChar; namelen:tOS_INT):tOS_INT;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;

  {Check Client}
  NetworkSetLastError(WSASYSNOTREADY);
  if DNSClient = nil then Exit;

  {Get Host Name}
  Result:=DNSClient.GetHostName(name,namelen);
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSANO_RECOVERY);
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: gethostname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getservbyport(port:tOS_INT; proto: PChar):PServEnt;
begin
 {}
 Result:=nil;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: getservbyport ' + E.Message);
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
  if WSStartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: getservbyname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function getprotobynumber(proto:tOS_INT):PProtoEnt;
begin
 {}
 Result:=nil;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: getprotobynumber ' + E.Message);
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
  if WSStartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: getprotobyname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function WSAStartup(wVersionRequired:word;var WSAData:TWSADATA):tOS_INT;
begin
 {}
 Result:=WSAEFAULT; {SOCKET_ERROR; See Spec}
 try
  {Set Result}
  Result:=WSASYSNOTREADY; {SOCKET_ERROR; See Spec}
  NetworkSetLastError(WSASYSNOTREADY);
  
  {Acquire the Lock}
  if CriticalSectionLock(WSStartupLock) = ERROR_SUCCESS then
   begin
    try
     {Check Count}
     if WSStartupCount <> 0 then
      begin
       {$IFDEF WINSOCK_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: WSAStartup additional call');
       {$ENDIF}
       
       {Check Version}
       Result:=WSAVERNOTSUPPORTED; {SOCKET_ERROR; See Spec}
       NetworkSetLastError(WSAVERNOTSUPPORTED);
       if wVersionRequired < WINSOCK_LOW_VERSION then Exit;
       if wVersionRequired > WINSOCK_HIGH_VERSION then Exit;
   
       {$IFDEF WINSOCK_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: WSAStartup checked version');
       {$ENDIF}
   
       {Increment Count}
       Inc(WSStartupCount);
   
       {Return Winsock Data}
       WSAData.wVersion:=Min(WINSOCK_HIGH_VERSION,wVersionRequired);
       WSAData.wHighVersion:=WINSOCK_HIGH_VERSION;
       WSAData.szDescription:=WINSOCK_NAME + ' (' + WINSOCK_BUILD_VERSION + ')';
       WSAData.szSystemStatus:='';
       WSAData.iMaxSockets:=WINSOCK_MAX_SOCKETS;
       WSAData.iMaxUdpDg:=WINSOCK_MAX_UDP;
       WSAData.lpVendorInfo:=nil;
       
       {Check Startup}
       if WSStartupError <> ERROR_SUCCESS then
        begin
         {Return Result}
         Result:=WSStartupError; {SOCKET_ERROR; See Spec}
         NetworkSetLastError(WSStartupError);
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
       {$IFDEF WINSOCK_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: WSAStartup initial call');
       {$ENDIF}
      
       {Check Version}
       Result:=WSAVERNOTSUPPORTED; {SOCKET_ERROR; See Spec}
       NetworkSetLastError(WSAVERNOTSUPPORTED);
       WSStartupError:=WSAVERNOTSUPPORTED;
       if wVersionRequired < WINSOCK_LOW_VERSION then Exit;
       if wVersionRequired > WINSOCK_HIGH_VERSION then Exit;
       {$IFDEF WINSOCK_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: WSAStartup checked version');
       {$ENDIF}
       
       {Increment Count}
       Inc(WSStartupCount);

       {Return Winsock Data}
       WSAData.wVersion:=Min(WINSOCK_HIGH_VERSION,wVersionRequired);
       WSAData.wHighVersion:=WINSOCK_HIGH_VERSION;
       WSAData.szDescription:=WINSOCK_NAME + ' (' + WINSOCK_BUILD_VERSION + ')';
       WSAData.szSystemStatus:='';
       WSAData.iMaxSockets:=WINSOCK_MAX_SOCKETS;
       WSAData.iMaxUdpDg:=WINSOCK_MAX_UDP;
       WSAData.lpVendorInfo:=nil;
    
       {Initialize Components}
       Result:=WSASYSNOTREADY; {SOCKET_ERROR; See Spec}
       NetworkSetLastError(WSASYSNOTREADY);
       WSStartupError:=WSASYSNOTREADY;
       
       {Start Sockets}
       if SocketsStart <> ERROR_SUCCESS then Exit;
       {$IFDEF WINSOCK_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: WSAStartup started sockets');
       {$ENDIF}
       
       {$IFDEF WINSOCK_DEBUG}
       if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: WSAStartup completed');
       {$ENDIF}
       
       {Return Result}
       Result:=ERROR_SUCCESS;  {NO_ERROR; See Spec}
       NetworkSetLastError(ERROR_SUCCESS);
       WSStartupError:=ERROR_SUCCESS;
      end; 
    finally
     {Release the Lock}
     CriticalSectionUnlock(WSStartupLock);
    end;
   end;
 except
  on E: Exception do
   begin
    Result:=WSAVERNOTSUPPORTED; {SOCKET_ERROR; See Spec}
    NetworkSetLastError(WSAVERNOTSUPPORTED);
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: WSAStartup ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

function WSACleanup:tOS_INT;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Set Error}
  NetworkSetLastError(WSASYSNOTREADY);
  
  {Acquire the Lock}
  if CriticalSectionLock(WSStartupLock) = ERROR_SUCCESS then
   begin
    try
     {Check Started}
     NetworkSetLastError(WSANOTINITIALISED);
     if WSStartupCount = 0 then Exit;
  
     {Decrement Count}
     Dec(WSStartupCount);
     Result:=NO_ERROR;
     NetworkSetLastError(ERROR_SUCCESS);
     if WSStartupCount > 0 then Exit;
  
     {$IFDEF WINSOCK_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: WSACleanup final call');
     {$ENDIF}
  
     {Shutdown and Cleanup}
     Result:=SOCKET_ERROR;
     NetworkSetLastError(WSAENETDOWN);
  
     {Stop Sockets}
     if SocketsStop <> ERROR_SUCCESS then Exit;
     {$IFDEF WINSOCK_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: WSACleanup stopped sockets');
     {$ENDIF}
  
     {$IFDEF WINSOCK_DEBUG}
     if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: WSACleanup completed');
     {$ENDIF}
 
     {Return Result}
     Result:=NO_ERROR;
     NetworkSetLastError(ERROR_SUCCESS);
     WSStartupError:=WSANOTINITIALISED;
    finally
     {Release the Lock}
     CriticalSectionUnlock(WSStartupLock);
    end;
   end;
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSANOTINITIALISED);
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: WSACleanup ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}

procedure WSASetLastError(iError:tOS_INT); inline;
begin
 {}
 NetworkSetLastError(iError);
end;

{==============================================================================}

function WSAGetLastError:tOS_INT; inline;
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

function WSAUnhookBlockingHook:tOS_INT;
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

function WSACancelBlockingCall:tOS_INT;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
 {Compatible with Winsock 2}
end;

{==============================================================================}

function WSAAsyncGetServByName(hWnd:HWND; wMsg:u_int; name:pchar; proto:pchar; buf:pchar; buflen:tOS_INT):THandle;
begin
 {}
 {Not Implemented}
 Result:=0; {As per Spec}
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAsyncGetServByPort(hWnd:HWND; wMsg:u_int; port:tOS_INT; proto:pchar; buf:pchar; buflen:tOS_INT):THandle;
begin
 {}
 {Not Implemented}
 Result:=0; {As per Spec}
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAsyncGetProtoByName(hWnd:HWND; wMsg:u_int; name:pchar; buf:pchar; buflen:tOS_INT):THandle;
begin
 {}
 {Not Implemented}
 Result:=0; {As per Spec}
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAsyncGetProtoByNumber(hWnd:HWND; wMsg:u_int; number:tOS_INT; buf:pchar; buflen:tOS_INT):THandle;
begin
 {}
 {Not Implemented}
 Result:=0; {As per Spec}
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAsyncGetHostByName(hWnd:HWND; wMsg:u_int; name:pchar; buf:pchar; buflen:tOS_INT):THandle;
begin
 {}
 {Not Implemented}
 Result:=0; {As per Spec}
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAsyncGetHostByAddr(hWnd:HWND; wMsg:u_int; addr:pchar; len:tOS_INT; family:tOS_INT; buf:pchar; buflen:tOS_INT):THandle;
begin
 {}
 {Not Implemented}
 Result:=0; {As per Spec}
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSACancelAsyncRequest(hAsyncTaskHandle:THandle):tOS_INT; 
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAAsyncSelect(s: TSocket; hWnd:HWND; wMsg:u_int; lEvent:longint):tOS_INT; { really a c-long }
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSARecvEx(s: TSocket;var buf; len:tOS_INT; flags:ptOS_INT):tOS_INT;
begin
 {}
 {Not Implemented}
 Result:=SOCKET_ERROR;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function __WSAFDIsSet(s: TSocket; var FDSet:TFDSet):Bool;
begin
 {}
 Result:=FD_ISSET(s,FDSet);
end;

{==============================================================================}

function __WSAFDIsSet_(s: TSocket; var FDSet:TFDSet):tOS_INT;
begin
 {}
 Result:=0;
 
 if FD_ISSET(s,FDSet) then
  begin
   Result:=FDSet.fd_count;
  end;
end;

{==============================================================================}

function TransmitFile(hSocket: TSocket; hFile: THandle; nNumberOfBytesToWrite: DWORD; nNumberOfBytesPerSend: DWORD; lpOverlapped: POverlapped; lpTransmitBuffers: PTransmitFileBuffers; dwReserved: DWORD): BOOL; 
begin
 {}
 {Not Implemented}
 Result:=False;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function AcceptEx(sListenSocket, sAcceptSocket: TSocket; lpOutputBuffer: Pointer; dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD; var lpdwBytesReceived: DWORD; lpOverlapped: POverlapped): BOOL; 
begin
 {}
 {Not Implemented}
 Result:=False;
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

procedure GetAcceptExSockaddrs(lpOutputBuffer: Pointer; dwReceiveDataLength, dwLocalAddressLength, dwRemoteAddressLength: DWORD; var LocalSockaddr: PSockAddr; var LocalSockaddrLength: Integer; var RemoteSockaddr: PSockAddr; var RemoteSockaddrLength: Integer); 
begin
 {}
 {Not Implemented}
 NetworkSetLastError(WSAEOPNOTSUPP);
end;

{==============================================================================}

function WSAMakeSyncReply(Buflen,Error:Word):dword;
begin
 {}
 Result:=MakeLong(Buflen,Error);
end;

{==============================================================================}

function WSAMakeSelectReply(Event,Error:Word):dword;
begin
 {}
 Result:=MakeLong(Event,Error);
end;

{==============================================================================}

function WSAGetAsyncBuflen(Param:dword):Word;
begin
 {}
 Result:=LoWord(Param);
end;

{==============================================================================}

function WSAGetAsyncError(Param:dword):Word;
begin
 {}
 Result:=HiWord(Param);
end;

{==============================================================================}

function WSAGetSelectEvent(Param:dword):Word;
begin
 {}
 Result:=LoWord(Param);
end;

{==============================================================================}

function WSAGetSelectError(Param:dword):Word;
begin
 {}
 Result:=HiWord(Param);
end;

{==============================================================================}

procedure FD_CLR(Socket:TSocket; var FDSet:TFDSet);
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

function FD_ISSET(Socket:TSocket; var FDSet:TFDSet):Boolean;
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

procedure FD_SET(Socket:TSocket; var FDSet:TFDSet);
begin
 {}
 if FDSet.fd_count < FD_SETSIZE then
  begin
   FDSet.fd_array[FDSet.fd_count]:=Socket;
   Inc(FDSet.fd_count);
  end;
end;

{==============================================================================}

procedure FD_ZERO(var FDSet:TFDSet);
begin
 {}
 FDSet.fd_count:=0;
end;

{==============================================================================}
{==============================================================================}
{Winsock Undocumented Functions}
function WsControl(Proto:DWORD;Action:DWORD;pRequestInfo:Pointer;var pcbRequestInfoLen:DWORD;pResponseInfo:Pointer;var pcbResponseInfoLen:DWORD):Integer; 
var
 Protocol:TNetworkProtocol;
begin
 {}
 Result:=SOCKET_ERROR;
 try
  {Check Started}
  NetworkSetLastError(WSANOTINITIALISED);
  if WSStartupError <> ERROR_SUCCESS then Exit;
  
  NetworkSetLastError(WSAEPROTONOSUPPORT);
  
  //To Do //For those that are documented call WsControlEx with adjusted params //See Winsock2
 except
  on E: Exception do
   begin
    Result:=SOCKET_ERROR;
    NetworkSetLastError(WSAEPROTONOSUPPORT);
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: WsControl ' + E.Message);
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
  if WSStartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: getnetbyaddr ' + E.Message);
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
  if WSStartupError <> ERROR_SUCCESS then Exit;

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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: getnetbyname ' + E.Message);
    {$ENDIF}
   end;
 end;
end;

{==============================================================================}
{==============================================================================}
{Winsock Enhanced Functions}
function WsControlEx(Proto:DWORD;Action:DWORD;pRequestInfo:Pointer;var pcbRequestInfoLen:DWORD;pResponseInfo:Pointer;var pcbResponseInfoLen:DWORD):Integer; 
var
 Count:LongWord;
 Address:TAddressEntry;
 Adapter:TTransportAdapter;
 Binding:TTransportBinding;
 Protocol:TNetworkProtocol;
 Transport:TNetworkTransport;

 WSAIfRow:PWSAIfRow;
 WSAIfTable:PWSAIfTable;
 WSAIpAddrRow:PWSAIpAddrRow;
 WSAIpAddrTable:PWSAIpAddrTable;

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
  if WSStartupError <> ERROR_SUCCESS then Exit;

  {Set Error}
  NetworkSetLastError(WSAEPROTONOSUPPORT);
  
  {Check Proto}
  case Proto of
   IPPROTO_IP:begin
     {Set Error}
     NetworkSetLastError(WSAEOPNOTSUPP);
     
     //To Do //See Winsock2
     
     {Check Action}
     case Action of
      WSA_GETIFTABLE:begin
        {Get Transport}
        Transport:=TransportManager.GetTransportByType(AF_INET,PACKET_TYPE_IP,False,NETWORK_LOCK_READ); //To Do
        if Transport = nil then Exit;
        
        {Get IfTable}
        NetworkSetLastError(WSAEINVAL);
        if pcbRequestInfoLen < SizeOf(TWSAIfTable) then Exit;
        WSAIfTable:=PWSAIfTable(pRequestInfo);
        if WSAIfTable = nil then Exit;
        WSAIfTable.dwNumEntries:=0;
        
        {Scan Adapters} //To Do //Change to Bindings when Done ?? - No !
        //To Do //Use GetAdapterByNext etc
        (*Adapter:=TTransportAdapter(Transport.Adapters.First); //To Do
        while Adapter <> nil do
         begin
          Inc(WSAIfTable.dwNumEntries);
          if pcbRequestInfoLen < (SizeOf(TWSAIfTable) + (WSAIfTable.dwNumEntries * SizeOf(TWSAIfRow))) then Exit;
          
          {Get IfRow}
          WSAIfRow:=PWSAIfRow(@WSAIfTable.table[WSAIfTable.dwNumEntries - 1]);
          if WSAIfRow = nil then Exit;
          //WSAIfRow.wszName:=Adapter.Name; //To Do
          WSAIfRow.dwIndex:=Adapter.Index;
          WSAIfRow.dwType:=Adapter.ConfigType;
          WSAIfRow.dwMtu:=Adapter.Mtu;
          WSAIfRow.dwSpeed:=0;
          WSAIfRow.dwPhysAddrLen:=SizeOf(THardwareAddress);
          System.Move(Adapter.Hardware[0],WSAIfRow.bPhysAddr[0],SizeOf(THardwareAddress));
          
          {Get Next Adapter}
          Adapter:=TTransportAdapter(Adapter.Next);
         end;*)
         
        {Return Result} 
        Result:=NO_ERROR;
        NetworkSetLastError(ERROR_SUCCESS);
       end;
      WSA_GETIPADDRTABLE:begin
        {Get Transport}
        Transport:=TransportManager.GetTransportByType(AF_INET,PACKET_TYPE_IP,False,NETWORK_LOCK_READ); //To Do
        if Transport = nil then Exit;
        
        {Get IpAddrTable}
        NetworkSetLastError(WSAEINVAL);
        if pcbRequestInfoLen < SizeOf(TWSAIpAddrTable) then Exit;
        WSAIpAddrTable:=PWSAIpAddrTable(pRequestInfo);
        if WSAIpAddrTable = nil then Exit;
        WSAIpAddrTable.dwNumEntries:=0;
        
        {Scan Adapters} //To Do //Change to Bindings when Done ?? - Yes !
        //To Do //Use GetAdapterByNext etc
        (*Adapter:=TTransportAdapter(Transport.Adapters.First); //To Do
        while Adapter <> nil do
         begin
          Inc(WSAIpAddrTable.dwNumEntries);
          if pcbRequestInfoLen < (SizeOf(TWSAIpAddrTable) + (WSAIpAddrTable.dwNumEntries * SizeOf(TWSAIpAddrRow))) then Exit;
          
          {Get IpAddrRow}
          WSAIpAddrRow:=PWSAIpAddrRow(@WSAIpAddrTable.table[WSAIpAddrTable.dwNumEntries - 1]);
          if WSAIpAddrRow = nil then Exit;
          WSAIpAddrRow.dwAddr:=LongWordNtoBE(TIPTransportAdapter(Adapter).Address.S_addr);
          WSAIpAddrRow.dwIndex:=TIPTransportAdapter(Adapter).Index;
          WSAIpAddrRow.dwMask:=LongWordNtoBE(TIPTransportAdapter(Adapter).Netmask.S_addr); 
          WSAIpAddrRow.dwBCastAddr:=LongWordNtoBE(TIPTransportAdapter(Adapter).Directed.S_addr);
          WSAIpAddrRow.dwReasmSize:=0;
          
          {Get Next Adapter}
          Adapter:=TTransportAdapter(Adapter.Next);
         end;*)
         
        {Return Result} 
        Result:=NO_ERROR;
        NetworkSetLastError(ERROR_SUCCESS);
       end;
      WSA_SETIPSTATISTICS:begin
        {Get Transport}
        Transport:=TransportManager.GetTransportByType(AF_INET,PACKET_TYPE_IP,False,NETWORK_LOCK_READ); //To Do
        if Transport = nil then Exit;
        
        {Get IpStats}
        NetworkSetLastError(WSAEINVAL);
        if pcbRequestInfoLen < SizeOf(TWSAIpStats) then Exit;
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
          if WSAIpStats.dwDefaultTTL = 0 then Exit;
          if WSAIpStats.dwDefaultTTL > 512 then Exit;
          TIPTransport(Transport).DefaultTTL:=WSAIpStats.dwDefaultTTL;
         end;
         
        {Return Result} 
        Result:=NO_ERROR;
        NetworkSetLastError(ERROR_SUCCESS);
       end;
      WSA_SETIPTTL:begin
        {Get Transport}
        Transport:=TransportManager.GetTransportByType(AF_INET,PACKET_TYPE_IP,False,NETWORK_LOCK_READ); //To Do
        if Transport = nil then Exit;

        {Get Value}
        NetworkSetLastError(WSAEINVAL);
        if pcbRequestInfoLen < SizeOf(LongWord) then Exit;
        WSATTL:=LongWord(pRequestInfo);
        if WSATTL = 0 then Exit;
        if WSATTL > 512 then Exit;

        {Set Value}
        TIPTransport(Transport).DefaultTTL:=WSATTL;
         
        {Return Result} 
        Result:=NO_ERROR;
        NetworkSetLastError(ERROR_SUCCESS);
       end;
      WSA_GETINTERFACEINFO:begin
        {Get Transport}
        Transport:=TransportManager.GetTransportByType(AF_INET,PACKET_TYPE_IP,False,NETWORK_LOCK_READ); //To Do
        if Transport = nil then Exit;
        
        {Get IpInterfaceInfo}
        NetworkSetLastError(WSAEINVAL);
        if pcbRequestInfoLen < SizeOf(TWSAIpInterfaceInfo) then Exit;
        WSAIpInterfaceInfo:=PWSAIpInterfaceInfo(pRequestInfo);
        if WSAIpInterfaceInfo = nil then Exit;
        WSAIpInterfaceInfo.NumAdapters:=0;
        
        {Scan Adapters} //To Do //Change to Bindings when Done ?? - No !
        //To Do //Use GetAdapterByNext etc
        (*Adapter:=TTransportAdapter(Transport.Adapters.First); //To Do
        while Adapter <> nil do
         begin
          Inc(WSAIpInterfaceInfo.NumAdapters);
          if pcbRequestInfoLen < (SizeOf(TWSAIpInterfaceInfo) + (WSAIpInterfaceInfo.NumAdapters * SizeOf(TWSAIpAdapterIndexMap))) then Exit;
          
          {Get IpAdapterIndexMap}
          WSAIpAdapterIndexMap:=PWSAIpAdapterIndexMap(@WSAIpInterfaceInfo.Adapter[WSAIpInterfaceInfo.NumAdapters - 1]);
          if WSAIpAdapterIndexMap = nil then Exit;
          WSAIpAdapterIndexMap.Index:=Adapter.Index;
          //WSAIpAdapterIndexMap.Name:=Adapter.Name; //To Do
          
          {Get Next Adapter}
          Adapter:=TTransportAdapter(Adapter.Next);
         end;*)
         
        {Return Result} 
        Result:=NO_ERROR;
        NetworkSetLastError(ERROR_SUCCESS);
       end;
      WSA_GETNETWORKPARAMS:begin
        {Get Transport}
        Transport:=TransportManager.GetTransportByType(AF_INET,PACKET_TYPE_IP,False,NETWORK_LOCK_READ); //To Do
        if Transport = nil then Exit;
        
        {Get FixedInfo}
        NetworkSetLastError(WSAEINVAL);
        if pcbRequestInfoLen < SizeOf(TWSAFixedInfo) then Exit;
        WSAFixedInfo:=PWSAFixedInfo(pRequestInfo);
        if WSAFixedInfo = nil then Exit;
        StrLCopy(WSAFixedInfo.HostName,PChar(Transport.Manager.Settings.HostName),WSA_MAX_HOSTNAME_LEN);
        StrLCopy(WSAFixedInfo.DomainName,PChar(Transport.Manager.Settings.DomainName),WSA_MAX_DOMAIN_NAME_LEN);
        WSAFixedInfo.DnsServerList.Next:=nil;
        StrLCopy(WSAFixedInfo.DnsServerList.IpAddress.S,PChar(InAddrToString(InAddrToNetwork(TIPTransport(Transport).Nameservers[0]))),15);
         
        {Return Result} 
        Result:=NO_ERROR;
        NetworkSetLastError(ERROR_SUCCESS);
       end;
      WSA_GETADAPTERSINFO:begin
        {Get Transport}
        Transport:=TransportManager.GetTransportByType(AF_INET,PACKET_TYPE_IP,False,NETWORK_LOCK_READ); //To Do
        if Transport = nil then Exit;
        
        {Get IpAdapterInfo}
        NetworkSetLastError(WSAEINVAL);
        if pcbRequestInfoLen < SizeOf(TWSAIpAdapterInfo) then Exit;
        WSAIpAdapterInfo:=PWSAIpAdapterInfo(pRequestInfo);
        if WSAIpAdapterInfo = nil then Exit;
        WSAIpAdapterInfo.Next:=nil;
        Count:=0;
        
        {Scan Adapters} //To Do //Change to Bindings when Done ?? - No !
        //To Do //Use GetAdapterByNext etc
        (*Adapter:=TTransportAdapter(Transport.Adapters.First); //To Do
        while Adapter <> nil do
         begin
          Inc(Count);
          if pcbRequestInfoLen < (SizeOf(TWSAIpAdapterInfo) * Count) then Exit;
          
          {Get IpAdapterInfo}
          //WSAIpAdapterInfo.AdapterName:=TIPTransportAdapter(Adapter).Name;       //To Do
          //WSAIpAdapterInfo.Description:=TIPTransportAdapter(Adapter).Description; //To Do
          WSAIpAdapterInfo.AddressLength:=SizeOf(THardwareAddress);
          System.Move(TIPTransportAdapter(Adapter).Hardware[0],WSAIpAdapterInfo.Address[0],SizeOf(THardwareAddress));
          WSAIpAdapterInfo.Index:=TIPTransportAdapter(Adapter).Index;
          WSAIpAdapterInfo.DhcpEnabled:=0;
          if TIPTransportAdapter(Adapter).ConfigType = CONFIG_TYPE_DHCP then WSAIpAdapterInfo.DhcpEnabled:=1;
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
          Adapter:=TTransportAdapter(Adapter.Next);
          
          {Get Next IpAdapterInfo}
          if Adapter <> nil then
           begin
            WSAIpAdapterInfo.Next:=Pointer(PtrUInt(WSAIpAdapterInfo) + SizeOf(TWSAIpAdapterInfo));
            WSAIpAdapterInfo:=WSAIpAdapterInfo.Next;
           end;
         end;*)
         
        {Return Result} 
        Result:=NO_ERROR;
        NetworkSetLastError(ERROR_SUCCESS);
       end;
      WSA_IPRELEASEADDRESS:begin
        {Get Transport}
        Transport:=TransportManager.GetTransportByType(AF_INET,PACKET_TYPE_IP,False,NETWORK_LOCK_READ); //To Do
        if Transport = nil then Exit;

        {Get IpAdapterIndexMap}
        NetworkSetLastError(WSAEINVAL);
        if pcbRequestInfoLen < SizeOf(TWSAIpAdapterIndexMap) then Exit;
        WSAIpAdapterIndexMap:=PWSAIpAdapterIndexMap(pRequestInfo);
        if WSAIpAdapterIndexMap = nil then Exit;

        {Get Adapter} //To Do //Get Binding ??
        
         
        {Return Result} 
        Result:=NO_ERROR;
        NetworkSetLastError(ERROR_SUCCESS);
       end;
      WSA_IPRENEWADDRESS:begin
        {Get Transport}
        Transport:=TransportManager.GetTransportByType(AF_INET,PACKET_TYPE_IP,False,NETWORK_LOCK_READ); //To Do
        if Transport = nil then Exit;

        {Get IpAdapterIndexMap}
        NetworkSetLastError(WSAEINVAL);
        if pcbRequestInfoLen < SizeOf(TWSAIpAdapterIndexMap) then Exit;
        WSAIpAdapterIndexMap:=PWSAIpAdapterIndexMap(pRequestInfo);
        if WSAIpAdapterIndexMap = nil then Exit;

        {Get Adapter} //To Do //Get Binding ??
         
        {Return Result} 
        Result:=NO_ERROR;
        NetworkSetLastError(ERROR_SUCCESS);
       end;
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
    {$IFDEF WINSOCK_DEBUG}
    if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'Winsock: Exception: WsControlEx ' + E.Message);
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
 
 Count:=Winsock.recv(WSTextIOInputSocket,ACh,SizeOf(Char),0);
 if Count = SOCKET_ERROR then
  begin
   Winsock.closesocket(WSTextIOInputSocket);
   WSTextIOInputSocket:=INVALID_SOCKET;
   Exit;
  end
 else
  begin
   if Count = 0 then
    begin
     Winsock.closesocket(WSTextIOInputSocket);
     WSTextIOInputSocket:=INVALID_SOCKET;
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
    Count:=Winsock.send(WSTextIOOutputSocket,Pointer(PtrUInt(ABuffer) + PtrUInt(Offset))^,ACount - Offset,0);
    if Count = SOCKET_ERROR then
     begin
      Winsock.closesocket(WSTextIOOutputSocket);
      WSTextIOOutputSocket:=INVALID_SOCKET;
      Exit;
     end
    else
     begin
      if Count = 0 then
       begin
        Winsock.closesocket(WSTextIOOutputSocket);
        WSTextIOOutputSocket:=INVALID_SOCKET;
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
{Winsock Helper Functions}
function WinsockRedirectInput(s:TSocket):Boolean;
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
   
   WSTextIOInputSocket:=INVALID_SOCKET;
  end
 else
  begin
   {Start Redirection}
   TextIOReadCharHandler:=SysTextIOReadChar;
  
   WSTextIOInputSocket:=s;
  end;  
end;

{==============================================================================}

function WinsockRedirectOutput(s:TSocket):Boolean; 
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
   
   WSTextIOOutputSocket:=INVALID_SOCKET;
  end
 else
  begin
   {Start Redirection}
   TextIOWriteCharHandler:=SysTextIOWriteChar;
   TextIOWriteBufferHandler:=SysTextIOWriteBuffer;
  
   WSTextIOOutputSocket:=s;
  end;  
end;

{==============================================================================}

function WinsockErrorToString(AError:LongInt):String;
begin
 {}
 Result:='';
 case AError of 
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

  WSAHOST_NOT_FOUND:Result:='WSAHOST_NOT_FOUND';
  WSATRY_AGAIN:Result:='WSATRY_AGAIN';
  WSANO_RECOVERY:Result:='WSANO_RECOVERY';
  WSANO_DATA:Result:='WSANO_DATA';
  {WSANO_ADDRESS:Result:='WSANO_ADDRESS';}
 end;
end;

{==============================================================================}
{==============================================================================}

initialization
 WSInit;
 if WINSOCK_AUTOSTART then
  begin
   if not WINSOCK_ASYNCSTART then
    begin
     {Start Winsock}
     WSStart;
    end
   else
    begin
     {Schedule Worker}
     WorkerSchedule(250,TWorkerTask(WSAsyncStart),nil,nil); {Delay start to allow device initialization}
    end;
  end; 

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
